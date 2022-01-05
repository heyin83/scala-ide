package org.scalaide.internal.builder.zinc

import java.io.File
import java.util.Optional
import java.util.function.Supplier

import org.scalaide.logging.Logger

import sbt.internal.inc.Analysis
import sbt.internal.inc.FileAnalysisStore
import sbt.util.InterfaceUtil.jo2o
import xsbti.compile.AnalysisContents
import xsbti.compile.CompileAnalysis
import xsbti.compile.MiniSetup

object SbtUtils {
  object AnalysisContents {
    def unapply(ac: AnalysisContents): Option[(CompileAnalysis, MiniSetup)] = Option((ac.getAnalysis, ac.getMiniSetup))
  }

  def readCache(cacheFile: File): Option[(Analysis, MiniSetup)] =
    jo2o(FileAnalysisStore.text(cacheFile).get()).map(_ match {
      case AnalysisContents(a: Analysis, i) => (a, i)
      case AnalysisContents(a, _) => throw new RuntimeException(s"Expected that sbt analysis for $cacheFile is of type ${classOf[Analysis]} but was ${a.getClass}.")
    })

  def readAnalysis(cacheFile: File): Analysis =
    readCache(cacheFile).map(_._1).getOrElse(Analysis.empty)

  object NoPosition extends xsbti.Position {
    def line(): Optional[Integer] = Optional.empty()
    def lineContent(): String = ""
    def offset(): Optional[Integer] = Optional.empty()
    def pointer(): Optional[Integer] = Optional.empty()
    def pointerSpace(): Optional[String] = Optional.empty()
    def sourceFile(): Optional[File] = Optional.empty()
    def sourcePath(): Optional[String] = Optional.empty()
  }

  def defaultSbtLogger(logger: Logger): xsbti.Logger = new xsbti.Logger {
    override def error(msg: Supplier[String]) = logger.error(msg.get)
    override def warn(msg: Supplier[String]) = logger.warn(msg.get)
    override def info(msg: Supplier[String]) = logger.info(msg.get)
    override def debug(msg: Supplier[String]) = logger.debug(msg.get)
    override def trace(exc: Supplier[Throwable]) = logger.error("", exc.get)
  }
}
