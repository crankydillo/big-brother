package org.beeherd.metrics

import scala.xml.XML

import org.beeherd.cli.utils.Tablizer
import org.beeherd.client.XmlResponse
import org.beeherd.client.http._
import org.beeherd.metrics.sonar.{
  NumberMetric, ProjectMetrics, SonarMetrics
}
import org.beeherd.metrics.vcs.{
  CommitterMetrics, SubversionMetrics
}
import org.apache.http.auth.{
  AuthScope, UsernamePasswordCredentials
}
import org.joda.time.DateTime
import org.rogach.scallop._
import org.rogach.scallop.exceptions._

object App {

  private class Conf(arguments: Seq[String]) extends LazyScallopConf(arguments) {
    version("Project Metrics 1.0")
    val svnUrlBase = opt[String](
      "svn-url-base"
      , short = 's'
      , required = true
      , descr = "Subversion base URL to which the projects get appended."
    )
    val sonarUrl = opt[String](
      "sonar-server"
      , short = 'S'
      , descr = "Sonar URL"
    )
    val committers = opt[List[String]](
      "committers"
      , descr = "Committers for which metrics are wanted."
    )
    val since = opt[String](
      "since"
      , required = true
      , descr = "Starting date for captured metrics.  Format: yyyy-MM-dd"
    )
    val until = opt[String](
      "until"
      , required = true
      , descr = "Ending date for captured metrics.  Format: yyyy-MM-dd"
    )
    val username = opt[String](
      "user"
      , descr = "Username for Subversion and Sonar authentication."
    )
    val password = opt[String](
      "password"
      , descr = "Password for Subversion and Sonar authentication."
    )
    val projects = trailArg[List[String]](
      "projects"
      , required = true
      , descr = "Projects for which metrics are wanted."
    )
  }

  def main(args: Array[String]): Unit = {
    val conf = new Conf(args)
    conf.initialize {
      case Help(c) => conf.printHelp; System.exit(0)
      case Version => println("Project Metrics 1.0"); System.exit(0)
      case RequiredOptionNotFound(o) => {
        println(o + " is required."); 
        println("Use --help for more information")
        System.exit(1)
      }
      case ScallopException(m) => println(m); System.exit(1);
    }

    val svnUrlBase = conf.svnUrlBase.apply

    val (protocol, server, port, _) = HttpRequest.parseUrl(svnUrlBase)

    val apacheClient = ClientFactory.createClient
    val client = new HttpClient(apacheClient)

    conf.username.get match {
      case Some(u) => {
        apacheClient.getCredentialsProvider().setCredentials(
          new AuthScope(server, port)
          , new UsernamePasswordCredentials(u, conf.password.apply)
        )
      }
      case _ => {}
    }

    try {

      val since = DateTime.parse(conf.since.apply)
      val until = DateTime.parse(conf.until.apply)

      val vcsMetrics = 
        conf.username.get match {
          case Some(u) => new SubversionMetrics(svnUrlBase, u, conf.password.apply)
          case _ => new SubversionMetrics(svnUrlBase)
        }
       
      val projsMap = 
        conf.projects.apply.map { p =>
          val projs = 
            conf.committers.get match {
              // projects changed are really modules changed...
              case Some(l) if !l.isEmpty => vcsMetrics.projectsChanged(p, l.toSet, since, until)
              case _ => vcsMetrics.projectsChanged(p, since, until) 
            }
          p -> projs
        }

      projsMap.foreach { case (p, metrics) =>
        println(p)
        print(metrics)
        println
      }

      if (conf.sonarUrl.get.isDefined) {
        val projects = 
          for ( 
               (p, metricsMap) <- projsMap;
               (_, metrics) <- metricsMap;
               path <- metrics.projectsChanged
             ) yield path
 
        val gavRetriever = new MavenGAVRetriever(client)
        val sonar = new SonarMetrics(client, conf.sonarUrl.apply)
        val sonarMetricss = sonarMetrics(sonar, svnUrlBase, gavRetriever, 
          projects.map { case (p, _) => p }, since, until)

        print(sonarMetricss)
      }
    } finally {
      apacheClient.getConnectionManager.shutdown
    }
  }

  private def sonarMetrics(
    sonar: SonarMetrics
    , svnUrlBase: String
    , gavRetriever: MavenGAVRetriever
    , projects: List[String]
    , since: DateTime
    , until: DateTime
  ): Map[String, Either[String, ProjectMetrics]] = {
    val pathsWithSonarIds = 
      for ( 
           path <- projects;
           gav <- gavRetriever.gav(svnUrlBase + path + "/pom.xml")
         ) yield {
           (
             path
             , gav.group + ":" + gav.artifact + ":" + gav.version.replace("-SNAPSHOT", "")
           )
         }

    pathsWithSonarIds.map { case (path, resource) =>
      try {
        path -> Right(sonar.metrics(resource, since, until))
      } catch {
        case e: Exception => 
          path -> Left("Problem getting metrics for " + 
            resource + ".  " + e.getMessage)
      }
    }.toMap
  }

  private def indent(str: String): String = (" " * 4) + str

  private def print(
    metricsMap: Map[String, CommitterMetrics]
  ): Unit = {
    def printCommitterMetrics(metrics: CommitterMetrics): Unit = {
      val header = "VCS Metrics for " + metrics.committer
      println(header)
      println("-" * header.size)


      println(indent("Commits: " + metrics.commits))
      println()

      println(indent("Projects"))
      println(indent("--------"))
      metrics
      .projectsChanged
      .foreach { case (proj, commits) => 
        println(indent(proj + " (" + commits + ")")) 
      }
    }

    metricsMap
    .toList
    .sort ( _._1 < _._1)
    .foreach { case (_, metrics) => printCommitterMetrics(metrics); println(); println() }
  }

  private def print(projectMetrics: Map[String, Either[String, ProjectMetrics]]): Unit = {
    println("Sonar Metrics")
    println()
    val (haveMetrics, doesNotHaveMetrics) = 
      projectMetrics
      .toList
      .sortWith { _._1 < _._1 }
      .partition { case (p, ms) => ms.isRight }

    haveMetrics.foreach { case (path, metrics) => 
      println(path)
      println("-" * path.length)
      print(metrics.right.get, " " * 4)
      println()
    }

    if (!doesNotHaveMetrics.isEmpty) {
      val header = "Projects for which no Sonar metrics could be retrieved"
      println(header)
      println("-" * header.size)
      doesNotHaveMetrics.foreach { case (p, _) => println(indent(p)) }
    }
  }

  private def print(projectMetrics: ProjectMetrics, tab: String): Unit = {
    val tablizer = new Tablizer("  ");

    def metric1(nm: NumberMetric, label: String) = metric2(nm.begin, nm.end, label)

    def metric2(begin: Double, end: Double, label: String) = {
      def fmt(d: Double) = "%.3f" format d

      val change = 
        if (begin == 0) "N/A"
        else fmt(((end - begin) / begin * 1.0)) 
      List(label, fmt(begin), fmt(end), change)
    }

    val headers = List("Metric", "Begin", "End", "% Change")
    val data = List(
      metric1(projectMetrics.linesOfCode, "Lines of Code (LOC)")
      , metric1(projectMetrics.testCoverage, "% Test Coverage")
    )
    val rows = tablizer.tablize(data, headers)

    rows.foreach { r => println(tab + r.mkString) }
  }

  sealed case class MavenGAV(group: String, artifact: String, version: String)

  private class MavenGAVRetriever(httpClient: HttpClient) {
    def gav(url: String): Option[MavenGAV] = {
      val resp = httpClient.get(url)
      if (resp.code < 299) {
        val xml = XML.loadString(resp.content.get.toString)

        // Try to get from itself, and then from parent
        def text(name: String) = {
          val tmp = (xml \ name).text.trim
          if (tmp != "") tmp
          else (xml \ "parent" \ name).text.trim
        }

        Some(MavenGAV(text("groupId"), text("artifactId"), text("version")))
      } else {
        None
      }
    }
  }
}
