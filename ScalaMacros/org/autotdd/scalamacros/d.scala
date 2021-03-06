package org.autotdd.scalamacros
import scala.reflect.macros.Context

import scala.language.experimental.macros

trait Demo6 {
  type T

  def add(param: Any): T = macro Demo6.addImpl[T]

  def fullAdd(param: Any, toStringBasedOnAST: String): T = {
    doesSomeThing_and_returnsSomething_OfTypeT
  }
  def doesSomeThing_and_returnsSomething_OfTypeT: T //just to allow compilation
}

object Demo6 {
  def addImpl[T: c.WeakTypeTag](c: Context)(param: c.Expr[Any]): c.Expr[T] = {
    import c.universe._
    val expr = reify {
      (c.Expr[Demo6](c.prefix.tree)).splice.fullAdd(param.splice,
        c.literal(show(param.tree)).splice)
    }
    c.Expr[T](expr.tree)
  }
}

