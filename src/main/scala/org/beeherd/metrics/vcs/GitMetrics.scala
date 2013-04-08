package org.beeherd.metrics.vcs

import java.io.File

import scala.xml.{
  Elem, XML
}

import org.joda.time.DateTime

/**
 * [[VCSMetrics]] implementation for [[http://subversion.tigris.org/ Subversion]].
 * 
 * [[http://svnbook.red-bean.com/en/1.7/svn.ref.svn.html Subversion's CLI]] must
 * be installed.  If the path to the svn command is not specified, it must be
 * added to your environment's PATH variable.
 *
 * @param logRetriever  [[LogRetriever]] that will give us the SVN log as XML
 */
class GitMetrics private[vcs] () extends VCSMetrics {
  def projectsChanged(
    project: String
    , since: DateTime
    , until: DateTime
  ): Map[String, CommitterMetrics] = {
    Map.empty
  }

  private class GitLILogRetriever(
    gitPath: String = "git"
  ) {
    import scala.sys.process._

    def log(project: String, since: DateTime, until: DateTime): String = {
      val projDir = new File(project)
      require(projDir.isDirectory)

      val cmd = gitPath + " log --pretty --date=short  --name-only " +
          " --since " + since + " --until " + until

      cmd!!
    }
  }
}
