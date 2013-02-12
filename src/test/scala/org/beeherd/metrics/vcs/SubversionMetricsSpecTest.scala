package org.beeherd.metrics.vcs


import org.mockito.Matchers._
import org.joda.time.DateTime
import org.specs2.matcher.ThrownExpectations
import org.specs2.mock._
import org.specs2.mutable._

class SubversionMetricsSpecTest 
extends SpecificationWithJUnit 
with Mockito 
with ThrownExpectations {

  private val sampleSvnLog =
    <log>
      <logentry revision="3">
        <author>bbob</author>
        <date>2007-03-31T22:45:20.836663Z</date>
        <msg>Billy's convinced.</msg>
        <paths>
          <path kind="file" action="A">java-pays-the-bills/src/main/scala/B.scala</path>
          <path kind="file" action="A">java-pays-the-bills/src/test/scala/BSpec.scala</path>
        </paths>
      </logentry>
      <logentry revision="2">
        <author>jdoe</author>
        <date>2007-03-31T22:45:20.836663Z</date>
        <msg>As are better in Scala</msg>
        <paths>
          <path kind="file" action="A">scala-rules/src/main/scala/A.scala</path>
          <path kind="file" action="A">scala-rules/src/test/scala/ASpec.scala</path>
        </paths>
      </logentry>
      <logentry revision="3">
        <author>bbob</author>
        <date>2007-03-31T20:53:24.764652Z</date>
        <msg>Billy makes his money off Java</msg>
        <paths>
          <path kind="file" action="A">java-pays-the-bills/src/main/java/B.java</path>
          <path kind="file" action="A">java-pays-the-bills/src/test/java/BTest.java</path>
          <path kind="file" action="A">play-around/src/main/scala/HelloWorld.scala</path>
        </paths>
      </logentry>

    </log>

  "SubversionMetrics" should {
    "return a mapping of committer to committer metrics" >> {
      val logRetriever = mock[LogRetriever]
      logRetriever.log(anyString, any[DateTime], any[DateTime]) returns sampleSvnLog
      val metricsMap = new SubversionMetrics(logRetriever).projectsChanged(
        "hi", new DateTime, new DateTime)

      metricsMap must haveSize(2)
      metricsMap must haveKey("bbob")
      metricsMap must haveKey("jdoe")

      val billy = metricsMap("bbob")

      "where the metrics for a user include" >> {
        "the author's name" >> {
          billy.committer mustEqual "bbob"
        }

        "the number of commits made by the author" >> {
          billy.commits mustEqual 2
        }

        "the projects that the author touched" >> {
          billy.projectsChanged must containTheSameElementsAs(
            List("java-pays-the-bills", "play-around"))
        }
      }
    }
  }
}
