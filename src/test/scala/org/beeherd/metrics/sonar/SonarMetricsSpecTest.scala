package org.beeherd.metrics.sonar


import org.mockito.Matchers._
import org.joda.time.DateTime
import org.specs2.matcher.ThrownExpectations
import org.specs2.mock._
import org.specs2.mutable._

import org.beeherd.client._
import org.beeherd.client.http._

class SonarMetricsSpecTest 
extends SpecificationWithJUnit 
with Mockito 
with ThrownExpectations {

  private val sampleJson = """
[ { "cells" : [ { "d" : "2013-02-07T03:54:15-0600",
          "v" : [ 15.9, 100, 23 ]
        },
        { "d" : "2013-02-08T03:57:26-0600",
          "v" : [ 16.1, 200, 21 ]
        }
      ],
    "cols" : [ { "metric" : "coverage" },
        { "metric" : "ncloc" },
        { "metric" : "violations" }
      ]
  } ]""".trim

  "SonarMetrics" should {

    "use the web service to retrieve the metrics between 2 dates for some " +
    "project" >> {
      val from = DateTime.parse("2012-01-01")
      val to = DateTime.parse("2012-02-01")

      "when the service returns a 200 response return metrics that include" >> {
        val client = clientt(StringResponse(sampleJson))
        val sonar = new SonarMetrics(client, "http://foo")
        val metrics = sonar.metrics("foo", from, to)

        val url = "http://foo/api/timemachine" +
          "?resource=foo" +
          "&fromDateTime=2012-01-01" +
          "&toDateTime=2012-02-01" +
          "&metrics=coverage,ncloc,violations"

        // Understandably, Mockito doesn't deal with Scala's default arguments
        // well.  Passing in nulls for defaults *seems* to work.
        there was one(client).get(url, null, null)

        "the project name" >> {
          metrics.projectName mustEqual "foo"
        }

        "beginning and ending values for" >> {
          "lines of code" >> {
            metrics.linesOfCode.begin mustEqual 100 
            metrics.linesOfCode.end mustEqual 200 
          }

          "unit test coverage" >> {
            metrics.testCoverage.begin mustEqual 15.9 
            metrics.testCoverage.end mustEqual 16.1 
          }
        }
      }

      "throw an exception if the service returns a non-200 response" >> {
        val client = clientt(BadResponse("oh nos"))
        val sonar = new SonarMetrics(client, "http://foo")

        sonar.metrics("foo", from, to) must throwA[RuntimeException]
      }
    }
  }

  private def clientt(resp: Response = OkResponse) = {
    val client = mock[HttpClient]
    client.get(anyString, any[Map[String,String]], 
      any[Map[String,List[String]]]) returns resp
    client
  }


}
