package org.scalaide.internal.builder.zinc

import org.scalaide.core.IScalaProject

/**
 * Entities that have reference to the Scala project
 */
trait HasScalaProject {
  val project: IScalaProject
}