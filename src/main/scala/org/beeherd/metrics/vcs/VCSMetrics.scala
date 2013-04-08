package org.beeherd.metrics.vcs

import org.joda.time.DateTime

/**
 * An interface that represents metrics obtained from version control systems.
 */
trait VCSMetrics {

  /**
   * Retrieve the [[CommitterMetrics metrics]], by committer, for a project.
   *
   * @param project     the relative path to the Subversion project
   * @param since       start of period for gathered metrics
   * @param until       end of period for gathered metrics
   * @return            metrics, by committer, for the project
   */
  def projectsChanged(
    project: String
    , since: DateTime
    , until: DateTime
  ): Map[String, CommitterMetrics]


  /**
   * Retrieve the [[CommitterMetrics metrics]], by committer, for a project.
   *
   * @param project     the relative path to the Subversion project
   * @param committers  only consider commits by these programmers 
   * @param since       start of period for gathered metrics
   * @param until       end of period for gathered metrics
   * @return            a mapping from committer name to committer metrics
   */
  def projectsChanged(
    project: String
    , committers: Set[String]
    , since: DateTime
    , until: DateTime
  ): Map[String, CommitterMetrics] =
    projectsChanged(project, since, until)
    .filter { case (committer, _) => committers.contains(committer) }

  /**
   * Retrieve the [[CommitterMetrics metrics]] for a project.
   *
   * @param project     the relative path to the Subversion project
   * @param committer   only consider commits by this programmer
   * @param since       start of period for gathered metrics
   * @param until       end of period for gathered metrics
   * @return            metrics for the given committer
   */
  def projectsChanged(
    project: String
    , committer: String
    , since: DateTime
    , until: DateTime
  ): CommitterMetrics = 
    projectsChanged(project, Set(committer), since, until)(committer)
}

case class CommitterMetrics(
  committer: String
  , commits: Int
  , projectsChanged: Set[(String, Int)]  // Path and number of commits
)
