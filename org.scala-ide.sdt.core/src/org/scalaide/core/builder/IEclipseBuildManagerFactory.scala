package org.scalaide.core.builder

import org.scalaide.core.IScalaProject
import scala.tools.nsc.Settings

trait IEclipseBuildManagerFactory {

  /**
   * Creates new Scope based build manager
   */
  def getScopesBuildManager(owningProject: IScalaProject, managerSettings: Settings): EclipseBuildManager

  /**
   * Creates new build manager which compiles sources without dividing on scopes
   */
  def getProjectsDependentBuildManager(owningProject: IScalaProject, managerSettings: Settings): EclipseBuildManager

}