package org.beeherd.metrics.sonar

import scala.util.parsing.json.JSON.parseFull

import org.beeherd.client.Response
import org.beeherd.client.http.HttpClient
import org.joda.time.DateTime

/**
 * [[http://www.sonarsource.org Sonar]] is a project that collects metrics from
 * various anaysis tools, such as [[http://findbugs.sourceforge.net/ Findbugs]]
 * and [[http://cobertura.sourceforge.net/ Cobertura]].  This class uses Sonar's
 * REST API in order to present these metrics in various forms.
 *
 * @param httpClient  client used for communicating with Sonar server
 * @param urlPrefix   prefix to use for URLs to the Sonar server (e.g.
 *                    http://your-sonar-server)
 */
class SonarMetrics(
  httpClient: HttpClient
  , urlPrefix: String
) {

  private val trimmedUrlPrefix = 
    if (urlPrefix.endsWith("/")) urlPrefix.dropRight(1)
    else urlPrefix

  /**
   * Retrieve metrics between two dates.
   */
  def metrics(project: String, since: DateTime, until: DateTime): ProjectMetrics = {
    def format(d: DateTime) = d.toString("yyyy-MM-dd")

    val url = trimmedUrlPrefix + "/sonar/api/timemachine" +
      "?resource=" + project +
      "&fromDateTime=" + format(since) +
      "&toDateTime=" + format(until) + 
      "&metrics=coverage,ncloc"

      println(url)

    val resp = httpClient.get(url)

    if (resp.code != 200)
      throw new RuntimeException("Unexpected HTTP response.  " + resp)

    toMetrics(project, resp)
  }

  private def toMetrics(projName: String, resp: Response): ProjectMetrics = {
    if (resp.content.isEmpty)
      throw new RuntimeException("The server's response did not have a JSON body.")
      
    parse(projName, resp.content.get.toString)
  }

  private def parse(projName: String, json: String): ProjectMetrics = {
    parseFull(json) match {
      case Some(l: List[Map[String, Any]]) => {
        val sz = l.size
        if (sz != 1) {
          println("Expected the list of metrics to be 1, but was " + 
             sz + ".  What's up with that?")
        }

        val metricNames = l(0)("cols").asInstanceOf[List[Map[String, Any]]].map { _("metric") }  // yuck

        val cells = l(0)("cells").asInstanceOf[List[Map[String, Any]]]
        val first = cells.head
        val last = cells.last

        val metrics = 
           metricNames.zipWithIndex.map { case (n, idx) => 
              NumberMetric(n + ""
                 , first("v").asInstanceOf[List[Double]](idx) 
                 , last("v").asInstanceOf[List[Double]](idx)
              )
           }
        val locs = {
            val m = metrics.find(m => m.name == "ncloc").get
            NumberMetric("lines-of-code", m.begin.toDouble, m.end.toDouble)
        }
        val coverages = {
            val m = metrics.find(m => m.name == "coverage").get
            NumberMetric("test-coverage", m.begin.toDouble, m.end.toDouble)
        }
        def parse = (a:Any) => DateTime.parse(a + "")
        ProjectMetrics(projName, parse(first("d")), parse(last("d")), locs, coverages, metrics)
      }
      case s => throw new RuntimeException("Expected a JSON List, not " + s)
    }
  }
}

abstract class Metric
case class NumberMetric(name: String, begin: Double, end: Double) extends Metric

case class ProjectMetrics(
  projectName: String
  , start: DateTime
  , end: DateTime
  , linesOfCode: NumberMetric = NumberMetric("loc", 0, 0)
  , testCoverage: NumberMetric = NumberMetric("coverage", 0, 0)
  , metrics: List[Metric] = Nil
) {
  def linesCoveredStart = linesOfCode.begin * testCoverage.begin
  def linesCoveredEnd = linesOfCode.end * testCoverage.end
}

