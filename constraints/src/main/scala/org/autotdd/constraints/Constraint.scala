package org.autotdd.constraints

import scala.reflect.macros.Context
import scala.language.experimental.macros

abstract class Constraint[B, RFn, R, C](val expected: R, val code: CodeFn[RFn, C], val because: Because[B]) {
  def params: List[Any]
  def actualValueFromParameters : R
  def hasDefaultBecause = because.becauseString == "true" //TODO replace hasDefaultBecause with better strategy. Magic strings not so good
}

case class CodeFn[RFn, C](val rfn: RFn, val description: String, val constraints: List[C] = List[C]())

object CodeFn {
  implicit def r_to_result[RFn, C](r: RFn): CodeFn[RFn, C] = macro c_to_code_impll[RFn, C]

  def c_to_code_impll[RFn: c.WeakTypeTag, C: c.WeakTypeTag](c: Context)(r: c.Expr[RFn]): c.Expr[CodeFn[RFn, C]] = {
    import c.universe._
    reify { CodeFn[RFn, C](r.splice, c.literal(show(r.tree)).splice) }
  }

}
case class Because[B](val because: B, val becauseString: String)

object Because {
  implicit def b_to_because[B](b: B): Because[B] = macro b_to_because_imp[B]

  def b_to_because_imp[B: c.WeakTypeTag](c: Context)(b: c.Expr[B]): c.Expr[Because[B]] = {
    import c.universe._
    reify { Because[B](b.splice, c.literal(show(b.tree)).splice) }
  }

}