package org.beeherd.metrics.vcs

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
class SubversionMetrics private[vcs] (
  logRetriever: LogRetriever
) extends VCSMetrics {

  /**
   * @param urlPrefix   prefix to use for URLs to the Subversion server (e.g.
   *                    http://your-sonar-server)
   * @param user        Subversion user
   * @param password    Subversion password
   * @param svnPath     path to the subversion command (default's to 'svn')
   */
  def this(
     urlPrefix: String
    , user: String
    , password: String
    , svnPath: String = "svn"
  ) = this(new SvnCLILogRetriever(urlPrefix, svnPath, Some(User(user, password))))

  def this(urlPrefix: String, svnPath: String = "svn") = 
    this(new SvnCLILogRetriever(urlPrefix, svnPath))

  def projectsChanged(
    project: String
    , since: DateTime
    , until: DateTime
  ): Map[String, CommitterMetrics] = {

    val xml = logRetriever.log(project, since, until)

    def projs(paths: List[String]) =
      paths.map(_.split("src")).filter(_.size == 2).map(_(0)).toSet

    (xml \ "logentry")
    .groupBy { e => (e \ "author").text.trim }
    .map { case (a, es) => 
      a -> CommitterMetrics(
          a, es.size,
          es
          .flatMap { e => (e \ "paths" \ "path").map { _.text.trim }}
          .map { _.split("/src/") }
          .filter { _.size == 2 }
          .map { _(0) }
          .toSet
        )
    }.toMap
  }
}

// Retrieves the Subversion log in XML form
private[this] trait LogRetriever {
  def log(project: String, since: DateTime, until: DateTime): Elem 
}

case class User(username: String, password: String)

// An implementation that uses the svn CLI
private[this] class SvnCLILogRetriever(
  urlPrefix: String
  , svnPath: String = "svn"
  , user: Option[User] = None
) extends LogRetriever {
  import scala.sys.process._

  // TODO DRY anyone??  See sonar.SonarMetrics
  private val trimmedUrlPrefix = 
    if (urlPrefix.endsWith("/")) urlPrefix.dropRight(1)
    else urlPrefix

  def log(project: String, since: DateTime, until: DateTime): Elem = {
    def format(date: DateTime) = date.toString("yyyy-MM-dd")
    val realProject = if (project.startsWith("/")) project else "/" + project

    val maybeUserInfo = user match {
      case Some(User(u, p)) => " --username " + u + " --password " + p
      case _ => ""
    }

    val cmd = svnPath + maybeUserInfo +
        " log -v -r {" + format(since) + "}:{" + format(until) + "} --xml " +
        trimmedUrlPrefix + realProject

    XML.loadString(cmd!!)
  }
}

