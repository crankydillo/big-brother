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

    val url = trimmedUrlPrefix + "/api/timemachine" +
      "?resource=" + project +
      "&fromDateTime=" + format(since) +
      "&toDateTime=" + format(until) + 
      "&metrics=coverage,ncloc,violations"

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
              DoubleMetric(n + ""
                 , first("v").asInstanceOf[List[Double]](idx) 
                 , last("v").asInstanceOf[List[Double]](idx)
              )
           }
        val locs = {
            val m = metrics.find(m => m.name == "ncloc").get
            IntMetric("lines-of-code", m.begin.toInt, m.end.toInt)
        }
        val coverages = {
            val m = metrics.find(m => m.name == "coverage").get
            DoubleMetric("test-coverage", m.begin.toDouble, m.end.toDouble)
        }
        val violations = {
            val m = metrics.find(m => m.name == "violations").get
            IntMetric("violations", m.begin.toInt, m.end.toInt)
        }
        def parse = (a:Any) => DateTime.parse(a + "")
        ProjectMetrics(projName, parse(first("d")), parse(last("d")), locs, 
          coverages, violations, metrics)
      }
      case s => throw new RuntimeException("Expected a JSON List, not " + s)
    }
  }
}

sealed abstract class Metric(name: String)
case class DoubleMetric(name: String, begin: Double, end: Double) extends Metric(name)
case class IntMetric(name: String, begin: Int, end: Int) extends Metric(name)

case class ProjectMetrics(
  projectName: String
  , start: DateTime
  , end: DateTime
  , linesOfCode: IntMetric = IntMetric("loc", 0, 0)
  , testCoverage: DoubleMetric = DoubleMetric("coverage", 0, 0)
  , violations: IntMetric = IntMetric("violations", 0, 0)
  , metrics: List[Metric] = Nil
) {
  def linesCoveredStart = linesOfCode.begin * testCoverage.begin
  def linesCoveredEnd = linesOfCode.end * testCoverage.end
}

