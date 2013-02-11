package org.beeherd.metrics

import org.beeherd.client.http._
import org.beeherd.metrics.vcs.{
  CommitterMetrics, SecureSubversionMetrics
}
import org.apache.http.auth.{
  AuthScope, UsernamePasswordCredentials
}
import org.joda.time.DateTime

object App {
  def main(args: Array[String]): Unit = {
    val apacheClient = ClientFactory.createClient

    val server = "server"
    val port = 443
    val user = "user"
    val pass = "pass"

    apacheClient.getCredentialsProvider().setCredentials(
      new AuthScope(server, port)
      , new UsernamePasswordCredentials(user, pass)
    )

    try {
      val svnUrlPrefix = "server"
      val user = "user"
      val pass = "pass"

      val project = "project"
      val since = DateTime.parse("2012-11-14")
      val until = DateTime.parse("2013-02-14")

      val vcsMetrics = new SecureSubversionMetrics(svnUrlPrefix, user, pass)

      val metricsMap = vcsMetrics.projectsChanged(project, since, until)

      print(metricsMap)

      val client = new HttpClient(apacheClient)

      val resource = "sonar-resource"

      val url = server + "/sonar/api/timemachine" +
      "?resource=" + resource +
      "&metrics=coverage,ncloc&fromDateTime=2013-02-07"
      val r = client.get(url)

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
