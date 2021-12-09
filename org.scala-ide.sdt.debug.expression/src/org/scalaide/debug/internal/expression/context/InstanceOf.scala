/*
 * Copyright (c) 2015 Contributor. All rights reserved.
 */
package org.scalaide.debug.internal.expression
package context

import scala.annotation.tailrec
import scala.collection.JavaConverters._

import org.scalaide.debug.internal.expression.Names.Scala
import org.scalaide.debug.internal.expression.proxies.JdiProxy
import org.scalaide.debug.internal.expression.proxies.primitives.BooleanJdiProxy
import org.scalaide.debug.internal.expression.proxies.primitives.PrimitiveJdiProxy
import org.scalaide.debug.internal.expression.proxies.primitives.NullJdiProxy
import org.scalaide.debug.internal.expression.proxies.primitives.UnitJdiProxy

import com.sun.jdi.ArrayType
import com.sun.jdi.ClassType
import com.sun.jdi.InterfaceType

import TypeNames._

/**
 * Implements `isInstanceOfCheck` method used to mock `isInstanceOf`.
 */
private[context] trait InstanceOf {
  self: Proxyfier =>

  /**
   * Checks if value under proxy conforms to given type.
   *
   * WARNING - this method is used in reflective compilation.
   * If you change its name, package or behavior, make sure to change it also.
   *
   * @param proxy proxy to check
   * @param typeName name of type to check against
   * @param BooleanJdiProxy
   */
  final def isInstanceOfCheck(proxy: JdiProxy, typeName: String): BooleanJdiProxy =
    valueProxy(this.mirrorOf(isInstanceOf(proxy, fixScalaObjectType(typeName)))).asInstanceOf[BooleanJdiProxy]

  /**
   * Checks if proxy matches given type.
   * Handles null, Unit, primitives and delegates everything else to `handleObject`.
   */
  private def isInstanceOf(proxy: JdiProxy, typeName: String): Boolean = proxy match {
    case _: NullJdiProxy =>
      false
    case _: UnitJdiProxy =>
      typeName == fixScalaPrimitives(Scala.unitType)
    case _ if proxy.__type.name == Scala.boxedUnitType =>
      typeName == fixScalaPrimitives(Scala.unitType)
    case boxedProxy: PrimitiveJdiProxy[_, _, _] =>
      val scalaPrimitiveName = fixScalaPrimitives(javaNameToScalaName(boxedProxy.primitiveName))
      scalaPrimitiveName == typeName
    case other => handleObject(other, typeName)
  }

  /**
   * Checks if proxy matches given type.
   * Handles Classes, Interfaces and Arrays (no variance support for now).
   */
  private def handleObject(proxy: JdiProxy, typeName: String): Boolean = proxy.__type match {
    case array: ArrayType =>
      val scalaComponentType = fixScalaPrimitives(javaNameToScalaName(array.componentTypeName))
      // TODO add support for variance - this needs some integration with `MethodInvoker.conformsTo`
      typeName == Scala.Array(scalaComponentType)
    case interface: InterfaceType =>
      val parents: Set[String] = (interface +: interface.subinterfaces.asScala)
        .map(_.name).toSet
      parents.contains(typeName)
    case clazz: ClassType =>
      val parents: Set[String] = ((clazz +: clazz.allInterfaces.asScala) ++ superclasses(clazz))
        .map(_.name).toSet
      parents.contains(typeName)
  }

  private def superclasses(clazz: ClassType): Seq[ClassType] = {
    @tailrec def loop(clazz: ClassType, result: Seq[ClassType]): Seq[ClassType] = {
      val superclass = clazz.superclass
      if (superclass == null) result
      else loop(superclass, result :+ superclass)
    }
    loop(clazz, Seq.empty)
  }
}
