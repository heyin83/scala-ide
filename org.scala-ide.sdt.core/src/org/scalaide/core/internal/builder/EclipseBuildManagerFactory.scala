package org.scalaide.core.internal.builder

import java.util.concurrent.ConcurrentMap
import java.util.concurrent.ConcurrentHashMap
import org.eclipse.core.runtime.CoreException
import org.eclipse.core.runtime.Platform

import scala.tools.nsc.Settings

import org.scalaide.core.IScalaProject
import org.scalaide.core.builder.EclipseBuildManager
import org.scalaide.logging.HasLogger
import org.scalaide.core.builder.IEclipseBuildManagerFactory

object EclipseBuildManagerFactory extends HasLogger {

  private val EXTENSION_POINT = "org.scala-ide.sdt.core.buildManagerFactory"
  private val SCALA_VERSION_ATTRIBUTE = "scalaVersion"
  private val CLASS_ATTRIBUTE = "class"

  // Note: The map has to be thread-safe, since it can potentially be accessed by different threads at the same time
  private val registry: ConcurrentMap[String, IEclipseBuildManagerFactory] = new ConcurrentHashMap

  registerFactories()

  /**
   * Creates new Scope based build manager
   */
  def getScopesBuildManager(owningProject: IScalaProject, managerSettings: Settings): Option[EclipseBuildManager] = {
    getFactory(owningProject.effectiveScalaInstallation().version.versionString).map(_.getScopesBuildManager(owningProject, managerSettings))
  }

  /**
   * Creates new build manager which compiles sources without dividing on scopes
   */
  def getProjectsDependentBuildManager(owningProject: IScalaProject, managerSettings: Settings): Option[EclipseBuildManager] = {
    getFactory(owningProject.effectiveScalaInstallation().version.versionString).map(_.getProjectsDependentBuildManager(owningProject, managerSettings))
  }

  /**
   * Creates new Scope based build manager
   */
  private def getFactory(versionString: String): Option[IEclipseBuildManagerFactory] = {
    val factory = if (versionString.startsWith("2.")) {
      registry.get("2")
    } else if (versionString.startsWith("3.")) {
      registry.get("3")
    } else {
      null
    }
    Option[IEclipseBuildManagerFactory](factory)
  }

  private def registerFactories(): Unit = {
    val extensionPoint = Platform.getExtensionRegistry().getExtensionPoint(EXTENSION_POINT)
    if (extensionPoint != null) {
      val extensions = extensionPoint.getExtensions()
      for {
        extension <- extensions
        config <- extension.getConfigurationElements
        if config.isValid
      } try {
        val factory = config.createExecutableExtension(CLASS_ATTRIBUTE).asInstanceOf[IEclipseBuildManagerFactory]
        registerFactory(config.getAttribute(SCALA_VERSION_ATTRIBUTE), factory)
      } catch {
        case e: CoreException =>
          eclipseLog.error("Failed to register source file provider for extension point: " + extension, e)
      }
    }
  }

  private def registerFactory(scalaVersion: String, factory: IEclipseBuildManagerFactory): Unit = {
    if (registry containsKey scalaVersion) {
      eclipseLog.warn(
        "Build Manager Factory for scala version `%s` already exists. Registration of `%s` will hence be ignored.".format(scalaVersion, factory))
    } else {
      registry.put(scalaVersion, factory)
    }
  }

  // Note: we may need to implement the `IRegistryEventListener` if we want to support plugins that are started on the fly. This can be easily done
  //       via `Platform.getExtensionRegistry().addListener(...)`

}