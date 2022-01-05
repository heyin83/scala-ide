package org.scalaide.internal.builder.zinc.scala3

import java.io.File

import org.eclipse.core.runtime.IProgressMonitor

import scala.tools.nsc.Settings

import org.scalaide.core.builder.IEclipseBuildManagerFactory
import org.scalaide.core.IScalaProject
import org.scalaide.core.builder.EclipseBuildManager
import org.scalaide.internal.builder.zinc.ProjectsDependentSbtBuildManager
import org.scalaide.core.IScalaInstallation

class ScalaThreeSbtBuilderManagerFactory extends IEclipseBuildManagerFactory {

  def getScopesBuildManager(owningProject: IScalaProject, managerSettings: Settings): EclipseBuildManager =
    new ProjectsDependentSbtBuildManager(owningProject, managerSettings) {
      override protected def createCompilerBridge(monitor: IProgressMonitor): File =
        compilerBridgeForInstallation(project.effectiveScalaInstallation())
    }

  def getProjectsDependentBuildManager(owningProject: IScalaProject, managerSettings: Settings): EclipseBuildManager =
    new ProjectsDependentSbtBuildManager(owningProject, managerSettings) {
      override protected def createCompilerBridge(monitor: IProgressMonitor): File =
        compilerBridgeForInstallation(project.effectiveScalaInstallation())
    }

  def compilerBridgeForInstallation(si: IScalaInstallation): File = {
    si.compilerBridge.map(_.classJar.toFile()).getOrElse(null)
  }

}