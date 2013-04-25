package org.beeherd.metrics

import scala.xml.XML

import org.apache.log4j.Logger

import org.beeherd.cli.utils.{
  DotDotDotter, Tablizer
}
import org.beeherd.client.XmlResponse
import org.beeherd.client.http._
import org.beeherd.metrics.sonar._
import org.beeherd.metrics.vcs.{
  CommitterMetrics, SubversionMetrics
}
import org.joda.time.DateTime
import org.rogach.scallop._
import org.rogach.scallop.exceptions._

class BigBrotherApp

/**
 * A CLI that uses a VCS client to determine what projects, represented by
 * paths, have changed over some period of time.  If the projects (paths) can be
 * used to identify Sonar projects, Sonar metrics can be displayed.
 */
object BigBrotherApp {
  private val Log = Logger.getLogger(classOf[BigBrotherApp])
  private val DateFormat = "yyyy-MM-dd"

  private class Conf(arguments: Seq[String]) 
  extends LazyScallopConf(arguments, true) {
    version("Big Brother - Version Control System and Sonar Analysis 1.0")
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
      , descr = "Starting date for captured metrics.  Defaults to two weeks " +
      "before --until value. Format: yyyy-MM-dd"
    )
    val until = opt[String](
      "until"
      , noshort = true
      , descr = "Ending date for captured metrics.  Defaults to today.  " +
      "Format: yyyy-MM-dd"
    )
    val username = opt[String](
      "user"
      , descr = "Username for Subversion and Sonar authentication."
    )
    val password = opt[String](
      "password"
      , descr = "Password for Subversion and Sonar authentication."
    )

    val passwordPrompt = toggle("pp", descrYes = "Prompt for password.")

    val projects = opt[List[String]](
      "projects"
      , required = true
      , descr = "Projects for which metrics are wanted."
    )

    mutuallyExclusive(password, passwordPrompt)
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

    val svnUrlBase = trimUrl(conf.svnUrlBase.apply)

    val (protocol, server, port, _) = HttpRequest.parseUrl(svnUrlBase)

    val password = pwd(conf)

    val apacheClient = conf.username.get match {
      case Some(u) => ClientFactory.createClient(server, u, password.get, port)
      case _ => ClientFactory.createClient
    }
    val client = new HttpClient(apacheClient)

    try {
      val until = conf.until.get match {
        case Some(s) => DateTime.parse(s)
        case _ => new DateTime
      }
      val since = conf.since.get match {
        case Some(s) => DateTime.parse(s)
        case _ => until.minusWeeks(2)
      }

      println("Big Brother reporting on " + since.toString(DateFormat) + 
        " to " + until.toString(DateFormat))

      val vcsMetrics = 
        conf.username.get match {
          case Some(u) => new SubversionMetrics(svnUrlBase, u, password.get)
          case _ => new SubversionMetrics(svnUrlBase)
        }

      def retrieveVCSMetrics() = {
        conf.projects.apply.map { p =>
          val projs = 
            conf.committers.get match {
              // projects changed are really modules changed...
              case Some(l) if !l.isEmpty => 
                vcsMetrics.projectsChanged(p, l.toSet, since, until)
              case _ => vcsMetrics.projectsChanged(p, since, until) 
            }
          p -> projs
        }
      }

      val vcsDotter = new DotDotDotter(retrieveVCSMetrics,
        "Retrieving VCS Metrics")

      val projsMap = vcsDotter.execute()

      println()
      projsMap.foreach { case (p, metrics) =>
        println("VCS Metrics for " + p)
        println()
        print(metrics)
        println()
      }

      if (conf.sonarUrl.get.isDefined) {
        def retrieveSonarStats() = {
          val projects = 
            for ( 
                 (p, metricsMap) <- projsMap;
                 (_, metrics) <- metricsMap;
                 path <- metrics.projectsChanged
               ) yield path
   
          val gavRetriever = new MavenGAVRetriever(client)
          val sonar = new SonarMetrics(client, conf.sonarUrl.apply)
          sonarMetrics(sonar, svnUrlBase, gavRetriever, 
            projects.map { case (p, _) => p }, since, until)
        }

        val sonarMetricss = new DotDotDotter(retrieveSonarStats, 
          "Retrieving Sonar Stats").execute()

        println()
        print(sonarMetricss)
      }

    } catch {
      case e: Exception => 
        Console.err.println(e.getMessage)
        Log.error("Exception", e)
    } finally {
      apacheClient.getConnectionManager.shutdown
    }
  }

  private def trimUrl(url: String): String =
    if (url.endsWith("/")) url.dropRight(1)
    else url

  private def pwd(conf: Conf): Option[String] = {
    // TODO Research the security implications of storing a password in a string
    conf.password.get match {
      case Some(p) => Some(p)
      case _ =>
        if (conf.passwordPrompt.isSupplied) {
          Some(
            new String(System.console.readPassword("%s", "Password: "))
          )
        } else {
          None
        }
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

  private def print(metricsMap: Map[String, CommitterMetrics]): Unit = {
    def printCommitterMetrics(metrics: CommitterMetrics): Unit = {
      val header = metrics.committer
      println(header)
      println("-" * header.size)

      println(indent("Commits: " + metrics.commits))
      println()

      println(indent("Projects"))
      println(indent("--------"))
      metrics
      .projectsChanged
      .toList
      .sortBy { case (p, _) => p }
      .foreach { case (proj, commits) => 
        println(indent(proj + " (" + commits + ")")) 
      }
    }

    metricsMap
    .toList
    .sortBy { case (p, _) => p }
    .foreach { case (_, metrics) => printCommitterMetrics(metrics); println(); println() }
  }

  private def print(projectMetrics: Map[String, Either[String, ProjectMetrics]]): Unit = {
    println("Sonar Metrics")
    println()
    val (haveMetrics, doesNotHaveMetrics) = 
      projectMetrics
      .toList
      .partition { case (p, ms) => ms.isRight }

    haveMetrics
    .sortBy { case (p, _) => p }
    .foreach { case (path, metrics) => 
      println(path)
      println("-" * path.length)
      print(metrics.right.get, " " * 4)
      println()
    }

    if (!doesNotHaveMetrics.isEmpty) {
      val header = "Projects for which no Sonar metrics could be retrieved"
      println(header)
      println("-" * header.size)
      doesNotHaveMetrics
      .sortBy { case (p, _) => p }
      .foreach { case (p, _) => println(indent(p)) }
    }
  }

  private def print(projectMetrics: ProjectMetrics, tab: String): Unit = {
    val na = "N/A"

    def metric(metric: Metric, label: String) = {
      def fmt(d: Double) = "%.3f" format d

      def change(begin: Double, end: Double): String = {
        if (begin == 0) na
        else fmt(((end - begin) / begin * 100.0)) 
      }

      metric match {
        case IntMetric(_, b, e) => List(label, b + "", + e + "", change(b, e))
        case DoubleMetric(_, b, e) => List(label, fmt(b), fmt(e), change(b, e))
      }
    }

    val headers = List("Metric", "Begin", "End", "% Change")
    val data = List(
      metric(projectMetrics.linesOfCode, "Lines of Code (LOC)")
      , metric(projectMetrics.testCoverage, "% Test Coverage")
      , metric(projectMetrics.violations, "Violations")
    )
    val tablizer = new Tablizer("  ");
    val rows = tablizer.tablize(data, headers)

    def fmt(d: DateTime) = d.toString(DateFormat)

    println(indent("Dates: " + fmt(projectMetrics.start) + " - " +
      fmt(projectMetrics.end)))
    println()
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
