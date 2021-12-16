package org.scalaide.core

object SdtConstants {

  // flags to enable using "-D..=true" vm arguments
  private[core] final val HeadlessProperty = "sdtcore.headless"
  private[core] final val NoTimeoutsProperty = "sdtcore.notimeouts"

  // Eclipse ids

  final val PluginId = "org.scala-ide.sdt.core"
  final val AspectsPluginId = "org.scala-ide.sdt.aspects"
  final val DebuggerPluginId = "org.scala-ide.sdt.debug"
  final val ExpressionEvaluatorPluginId = "org.scala-ide.sdt.debug.expression"
  final val ScalaRefactoringPluginId = "org.scala-refactoring.library"
  final val LibraryPluginId = "org.scala-lang.scala-library"
  final val ZincPluginId = "org.scala-ide.zinc.library"
  final val ZincCompilerBridgePluginId = "org.scala-ide.zinc.compiler.bridge.source"
  final val EditorId = "scala.tools.eclipse.ScalaSourceFileEditor"
  final val ScalaPerspectiveId = "org.scala-ide.sdt.core.perspective"

  final val LaunchTypeId = "scala.application"

  // containers
  final val ScalaLibContId = "org.scala-ide.sdt.launching.SCALA_CONTAINER"
  final val ScalaLibContName = "Scala Library container"
  final val ScalaCompilerContId = "org.scala-ide.sdt.launching.SCALA_COMPILER_CONTAINER"
  final val ScalaCompilerContName = "Scala Compiler container"

  // project nature
  final val NatureId = "org.scala-ide.sdt.core.scalanature"

  // marker ids
  final val ProblemMarkerId = "org.scala-ide.sdt.core.problem"
  final val ClasspathProblemMarkerId = "org.scala-ide.sdt.core.classpathProblem"
  final val ScalaVersionProblemMarkerId = "org.scala-ide.sdt.core.scalaVersionProblem"
  final val SettingProblemMarkerId = "org.scala-ide.sdt.core.settingProblem"
  final val TaskMarkerId = "org.scala-ide.sdt.core.task"
  /** All Scala error markers. */
  final val ScalaErrorMarkerIds = Set(ClasspathProblemMarkerId, ProblemMarkerId, SettingProblemMarkerId)

  // builder
  final val BuilderId = "org.scala-ide.sdt.core.scalabuilder"

  // wizards
  final val ProjectWizId = "org.scala-ide.sdt.core.wizards.newProject"
  final val ClassCreatorWizId = "org.scalaide.ui.wizards.classCreator"
  final val TraitCreatorWizId = "org.scalaide.ui.wizards.traitCreator"
  final val ObjectCreatorWizId = "org.scalaide.ui.wizards.objectCreator"
  final val PackageObjectCreatorWizId = "org.scalaide.ui.wizards.packageObjectCreator"
  final val AppCreatorWizId = "org.scalaide.ui.wizards.appCreator"

  // file extensions
  final val ScalaFileExtn = ".scala"
  final val JavaFileExtn = ".java"

  final val IssueTracker = "https://www.assembla.com/spaces/scala-ide/support/tickets"
  final val SveltoHomepage = "https://github.com/dragos/svelto"

}
