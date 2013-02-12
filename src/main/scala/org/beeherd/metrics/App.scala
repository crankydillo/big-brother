package org.beeherd.metrics

import scala.xml.XML

import org.beeherd.client.XmlResponse
import org.beeherd.client.http._
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
    val svnUrlBase = opt[String]("svn-url-base", short = 's', required = true, 
      descr = "Subversion base URL to which the projects get appended.")
    val sonarUrl = opt[String]("sonar-server", short = 'S', required = true, 
      descr = "Sonar URL")
    val since = opt[String]("since", required = true,
      descr = "Starting date for captured metrics.  Format: yyyy-MM-dd")
    val until = opt[String]("until", required = true,
      descr = "Ending date for captured metrics.  Format: yyyy-MM-dd")
    val username = opt[String]("user", 
      descr = "Username for Subversion and Sonar authentication.")
    val password = opt[String]("password",
      descr = "Password for Subversion and Sonar authentication.")
    val projects = trailArg[List[String]]("projects", required = true, 
      descr = "Projects for which metrics are wanted.")
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

    System.exit(0)

    val (protocol, server, port, _) = HttpRequest.parseUrl(svnUrlBase)

    val apacheClient = ClientFactory.createClient

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

      val vcsMetrics = new SubversionMetrics(svnUrlBase)

      val project = "/servicemix/smx4/bundles/trunk/"
      val metricsMap = vcsMetrics.projectsChanged(project, since, until)

      print(metricsMap)

      val client = new HttpClient(apacheClient)

      metricsMap.get("ningjiang") match {
        case Some(metrics) => {
          val projects = metrics.projectsChanged
          val url = svnUrlBase + projects.toList.head + "/pom.xml"
          println(url)
          println(new MavenGAVRetriever(client).gav(url))
        }
        case _ => "couldn't find user:("
      }
      client

      /*
      val resource = "sonar-resource"

      val url = server + "/sonar/api/timemachine" +
      "?resource=" + resource +
      "&metrics=coverage,ncloc&fromDateTime=2013-02-07"
      val r = client.get(url)
      */

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
    def gav(url: String): MavenGAV = {
      val resp = httpClient.get(url)
      val xml = XML.loadString(resp.content.get.toString)
      def text(name: String) = (xml \ name).text.trim
      MavenGAV(text("groupId"), text("artifactId"), text("version"))
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
