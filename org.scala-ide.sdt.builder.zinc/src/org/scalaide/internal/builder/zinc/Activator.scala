package org.scalaide.internal.builder.zinc

import org.scalaide.core.SdtConstants
import org.scalaide.core.ScalaIdeDataStore
import org.scalaide.util.eclipse.OSGiUtils
import org.eclipse.core.runtime.Platform
import org.eclipse.core.runtime.Plugin
import org.eclipse.core.runtime.Path
import org.scalaide.internal.builder.zinc.scala2.CompilerBridgeStore

object Activator {

  @volatile private var plugin: Activator = _

  def apply(): Activator = plugin

}

class Activator extends Plugin {

  Activator.plugin = this

  lazy val zincCompilerBundle = Platform.getBundle(SdtConstants.ZincPluginId)
  lazy val zincCompilerBridgeBundle = Platform.getBundle(SdtConstants.ZincCompilerBridgePluginId)
  lazy val zincCompilerBridge = OSGiUtils.pathInBundle(zincCompilerBridgeBundle, "/")
  private lazy val compilerBridgeSrc = OSGiUtils.getBundlePath(zincCompilerBridgeBundle)
  private lazy val zincFullJar = OSGiUtils.getBundlePath(zincCompilerBundle)

  /** The compiler-bridge store, located in user data area */
  lazy val compilerBridgeStore: CompilerBridgeStore = new CompilerBridgeStore(
      new Path(ScalaIdeDataStore.dataStoreLocation),
      compilerBridgeSrc.getOrElse(null),
      zincCompilerBridgeBundle.getVersion.toString(),
      zincFullJar.getOrElse(null))

}
