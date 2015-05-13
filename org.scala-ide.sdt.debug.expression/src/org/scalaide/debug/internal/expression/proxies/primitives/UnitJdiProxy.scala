/*
 * Copyright (c) 2014 Contributor. All rights reserved.
 */
package org.scalaide.debug.internal.expression.proxies.primitives

import org.scalaide.debug.internal.expression.context.JdiContext
import org.scalaide.debug.internal.expression.proxies.JdiProxy

import com.sun.jdi.Value

/**
 * JdiProxy implementation for `java.lang.Void`.
 */
case class UnitJdiProxy(__context: JdiContext) extends JdiProxy {

  private def fail: Nothing = throw new UnsupportedOperationException("There are no methods on Unit.")

  override def __value: Value = fail

  /** Implementation of method application. */
  override def applyDynamic(name: String)(args: Any*): JdiProxy = fail

  /** Implementation of field selection. */
  override def selectDynamic(name: String): JdiProxy = fail

  /** Implementation of variable mutation. */
  override def updateDynamic(name: String)(value: Any): UnitJdiProxy = fail
}
