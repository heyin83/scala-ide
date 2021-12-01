/*
 * Copyright 2005-2010 LAMP/EPFL
 */

package scala.tools.refactoring
package common

import java.io.File
import java.io.FileOutputStream
import java.io.PrintStream

trait Tracing {
  protected abstract class TraceAndReturn[T] {
    def \\(trace: T => Unit): T
  }

  protected implicit def wrapInTraceAndReturn[T](t: T): TraceAndReturn[T]

  def context[T](name: String)(body: => T): T

  def trace(msg: => String, arg1: => Any, args: Any*): Unit

  def trace(msg: => String): Unit
}

object DebugTracing {
  private val debugStream = {
    Option(System.getProperty("scala.refactoring.traceFile")).flatMap { fn =>
      try {
        val traceFile = new File(fn)
        val out = new FileOutputStream(traceFile, true)
        Some(new PrintStream(out, true, "UTF-8"))
      } catch {
        case e: Exception =>
          e.printStackTrace()
          System.err.println(s"Could not open '$fn' for writing; falling back to System.out...")
          None
      }
    }.getOrElse(System.out)
  }

  private def printLine(str: String) = {
    debugStream.println(str)
  }
}

/**
 * Traces to STDOUT or a custom file (via the system property `scala.refactoring.traceFile`)
 */
trait DebugTracing extends Tracing {
  import DebugTracing._

  var level = 0
  val marker = "│"
  val indent = "   "

  override def context[T](name: String)(body: => T): T = {

    val spacer = "─" * (indent.length - 1)

    printLine((indent * level) +"╰"+ spacer +"┬────────" )
    level += 1
    trace("→ "+ name)

    body \\ { _ =>
      level -= 1
      printLine((indent * level) + "╭"+ spacer +"┴────────" )
    }
  }

  override def trace(msg: => String, arg1: => Any, args: Any*): Unit = {

    val as: Array[AnyRef] = arg1 +: args.toArray map {
      case s: String => "«"+ s.replaceAll("\n", "\\\\n") +"»"
      case a: AnyRef => a
    }

    trace(msg.format(as: _*))
  }

  override def trace(msg: => String): Unit = {
    val border = (indent * level) + marker
    printLine(border + msg.replaceAll("\n", "\n"+ border))
  }

  protected implicit final def wrapInTraceAndReturn[T](t: T) = new TraceAndReturn[T] {
    def \\(trace: T => Unit) = {
      trace(t)
      t
    }
  }
}

trait SilentTracing extends Tracing {
  @inline
  def trace(msg: => String, arg1: => Any, args: Any*) = ()

  @inline
  def trace(msg: => String) = ()

  @inline
  def context[T](name: String)(body: => T): T = body

  protected implicit final def wrapInTraceAndReturn[T](t: T) = new TraceAndReturn[T] {
    def \\(ignored: T => Unit) = t
  }
}
