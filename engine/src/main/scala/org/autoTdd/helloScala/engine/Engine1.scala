package org.autoTdd.helloScala.engine

import scala.language.experimental.macros
import scala.reflect.macros.Context
import org.autotdd.constraints.Constraint
import org.autotdd.constraints.CodeFn
import org.autotdd.constraints.Because

case class Constraint1[P, R](val param: P, override val expected: R, override val code: CodeFn[(P) => R], override val because: Because[(P) => Boolean])
  extends Constraint[(P) => Boolean, (P) => R, R](expected, code, because) {
  override def params = List(param)
}

object Engine1 {
  def apply[P, R](default: R = null) = new MutableEngine1[P, R](default)
  def mutable[P, R](default: R = null) = new MutableEngine1[P, R](default)
  def immutable[P, R](default: R = null) = new ImmutableEngine1[P, R](List())

  def blankTrue[P, R](r: R): (P) => Boolean = (p) => true;
  def blankR[P, R](r: R): (P) => R = (p) => r;

}

trait Engine1Types[P, R] extends EngineTypes[R] {
  type B = (P) => Boolean
  type RFn = (P) => R
  type C = Constraint1[P, R]

}

trait Engine1[P, R] extends Engine[R] with Function1[P, R] with EngineToString[R] with Engine1Types[P, R] {

  def apply(p: P): R = evaluate(b => b(p), root)(p)

  def assertion(p: P, expected: R): CR = constraint(p, expected)

  def constraint(p: P, expected: R, code: CodeFn[RFn] = null, because: Because[B] = Because[(P => Boolean)]((p: P) => true, "true")): CR = {
    if (code == null)
      addConstraint(realConstraint(Constraint1(p, expected, CodeFn[RFn]((p: P) => expected, expected.toString), because)))
    else
      addConstraint(realConstraint(Constraint1(p, expected, code, because)))
  }

  def makeClosureForBecause(params: List[Any]) = (b) => b(params(0).asInstanceOf[P])
  def makeClosureForResult(params: List[Any]) = (r) => r(params(0).asInstanceOf[P])

  def makeDefaultRoot(defaultRoot: R): RorN = Left(CodeFn[RFn]((p: P) => defaultRoot, defaultRoot.toString))
}

class ImmutableEngine1[P, R](val constraints: List[Constraint1[P, R]]) extends Engine1[P, R] {
  type CR = ImmutableEngine1[P, R]
  def root = null
  def addConstraint(c: Constraint1[P, R]) = this
}

class MutableEngine1[P, Result](val defaultValue: Result) extends MutableEngine[Result](defaultValue) with Engine1[P, Result] 