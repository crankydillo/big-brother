package org.beeherd.metrics

import scala.xml.XML

import org.beeherd.client.XmlResponse
import org.beeherd.client.http._
import org.beeherd.metrics.sonar.SonarMetrics
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
    val defaultVersion = opt[String](
      "default-version"
      , descr = "Default project version to use for Sonar analysis.  This is " +
      "required if the project's pom does not include version information."
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
        conf.username.get match {
          case Some(u) => {
            apacheClient.getCredentialsProvider().setCredentials(
              new AuthScope(server, port)
              , new UsernamePasswordCredentials(u, conf.password.apply)
            )
          }
          case _ => {}
        }

        val client = new HttpClient(apacheClient)
        val gavRetriever = new MavenGAVRetriever(client)
        val sonarIds = 
          for ( 
               (p, metricsMap) <- projsMap;
               (_, metrics) <- metricsMap;
               path <- metrics.projectsChanged;
               gav <- gavRetriever.gav(svnUrlBase + path + "/pom.xml")
             ) yield {
               val version = 
                 if (gav.version == "") {
                   conf.defaultVersion.get match {
                     case Some(v) => v
                     case _ => ""
                   }
                 } else {
                   gav.version
                 }
               gav.group + ":" + gav.artifact + ":" + version.replace("-SNAPSHOT", "")
             }

        val sonar = new SonarMetrics(client, conf.sonarUrl.apply)

        sonarIds.foreach { resource =>
          try {
            val metrics = sonar.metrics(resource, since, until)
            println(metrics)
          } catch {
            case e: Exception => println("Problem getting metrics for " + 
              resource + ".  " + e.getMessage)
          }
        }
      }

    } finally {
      apacheClient.getConnectionManager.shutdown
    }
  }

  private def print(metricsMap: Map[String, CommitterMetrics]): Unit = {
    def printCommitterMetrics(metrics: CommitterMetrics): Unit = {
      val header = "Metrics for " + metrics.committer
      println(header)
      println("-" * header.size)

      def indent(str: String): String = (" " * 4) + str

      println(indent("Commits: " + metrics.commits))
      println()

      println(indent("Projects"))
      println(indent("--------"))
      metrics.projectsChanged.foreach { p => println(indent(p)) }
    }

    metricsMap
    .toList
    .sort ( _._1 < _._1)
    .foreach { case (_, metrics) => printCommitterMetrics(metrics); println(); println() }
  }

  sealed case class MavenGAV(group: String, artifact: String, version: String)

  private class MavenGAVRetriever(httpClient: HttpClient) {
    // TODO externalize
    val replaceMap = Map(
      "rel_10_2_5" -> "10.2.5-SNAPSHOT"
    )

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

   /*
  private def print(metricsMap: Map[String, CommitterMetrics]): Unit = { 
    println(report.city + ", " + report.state);
    println();
    val tablizer = new Tablizer("  ");

    val headers = List(
      "Committerr, 
    )   

    val data = report.forecasts.map {f =>
      List(
        f.day + ""
        , f.high + ""
        , f.low + ""
        , f.conditions
        , f.rainChance + ""
        , f.averageHumidity + ""
        , f.maxWind + ""
      )   
    }   
    println(tablizer.tablizeToStr(data, headers))
  }
 */
}
