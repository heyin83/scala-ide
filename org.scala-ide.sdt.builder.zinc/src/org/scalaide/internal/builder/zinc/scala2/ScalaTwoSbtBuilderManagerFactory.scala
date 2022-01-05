package org.scalaide.internal.builder.zinc.scala2

import java.io.File

import org.eclipse.core.runtime.IProgressMonitor

import scala.tools.nsc.Settings

import org.scalaide.core.builder.IEclipseBuildManagerFactory
import org.scalaide.core.IScalaProject
import org.scalaide.core.builder.EclipseBuildManager
import org.scalaide.internal.builder.zinc.ProjectsDependentSbtBuildManager
import xsbti.compile.ScalaInstance
import org.scalaide.internal.builder.zinc.Activator

class ScalaTwoSbtBuilderManagerFactory extends IEclipseBuildManagerFactory {

  def getScopesBuildManager(owningProject: IScalaProject, managerSettings: Settings): EclipseBuildManager =
    new ProjectsDependentSbtBuildManager(owningProject, managerSettings) {
      override protected def createCompilerBridge(monitor: IProgressMonitor): File = {
        val version = project.effectiveScalaInstallation().version.unparse
        val instance = getScalaInstance()
        compilerBridgeForInstallation(version, instance,monitor)
      }
    }

  def getProjectsDependentBuildManager(owningProject: IScalaProject, managerSettings: Settings): EclipseBuildManager =
    new ProjectsDependentSbtBuildManager(owningProject, managerSettings) {
      override protected def createCompilerBridge(monitor: IProgressMonitor): File = {
        val version = project.effectiveScalaInstallation().version.unparse
        val instance = getScalaInstance()
        compilerBridgeForInstallation(version, instance,monitor)
      }
    }

  def compilerBridgeForInstallation(version: String, scalaInstance: ScalaInstance, monitor: IProgressMonitor): File = {
    val x = Activator().compilerBridgeStore.compilerBridgeFor(version,scalaInstance)(monitor).map(_.toFile())
    x.getOrElse(null)
  }

}