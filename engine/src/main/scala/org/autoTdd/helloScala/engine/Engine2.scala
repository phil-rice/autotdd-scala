package org.autoTdd.helloScala.engine

import scala.language.experimental.macros
import scala.reflect.macros.Context
import org.autotdd.constraints.Constraint
import org.autotdd.constraints.CodeFn
import org.autotdd.constraints.Because

case class Constraint2[P1, P2, R](val p1: P1, val p2: P2, override val expected: R, override val code: CodeFn[(P1, P2) => R, Constraint2[P1,P2,R]], override val because: Because[(P1, P2) => Boolean])
  extends Constraint[(P1, P2) => Boolean, (P1, P2) => R, R, Constraint2[P1,P2,R]](expected, code, because) {
  override def params = List(p1, p2)
  def actualValueFromParameters = code.rfn(p1, p2)
}

object Engine2 {
  def apply[P1, P2, R](default: R = null) = new MutableEngine2[P1, P2, R](default)
  def mutable[P1, P2, R](default: R = null) = new MutableEngine2[P1, P2, R](default)
  def immutable[P1, P2, R](default: R = null) = new ImmutableEngine2[P1, P2, R](default, List())
}
trait Engine2Types[P1, P2, R] extends EngineTypes[R] {
  type B = (P1, P2) => Boolean
  type RFn = (P1, P2) => R
  type C = Constraint2[P1, P2, R]

}

trait Engine2[P1, P2, R] extends Engine[R] with Function2[P1, P2, R] with EngineToString[R] with Engine2Types[P1, P2, R] {
  def apply(p1: P1, p2: P2): R = {
    val rFn = evaluate(b => b(p1, p2), root)
    rFn(p1, p2)
  }

  def makeClosureForBecause(params: List[Any]) = (b) => b(params(0).asInstanceOf[P1], params(1).asInstanceOf[P2])
  def makeClosureForResult(params: List[Any]) = (r) => r(params(0).asInstanceOf[P1], params(1).asInstanceOf[P2])

  def assertion(p1: P1, p2: P2, expected: R): CR = constraint(p1, p2, expected)

  def constraint(p1: P1, p2: P2, expected: R, code: Code = null, because: Because[B] = Because[(P1, P2) => Boolean]((p1, p2) => true, "true")): CR = {
    if (code == null)
      addConstraint(realConstraint(Constraint2(p1, p2, expected, CodeFn[RFn,C]((p1: P1, p2: P2) => expected, expected.toString), because)))
    else
      addConstraint(realConstraint(Constraint2(p1, p2, expected, code, because)))
  }

  def makeDefaultRoot(defaultRoot: R): RorN =
    Left(CodeFn((p1, p2) => defaultRoot, defaultRoot match {
      case null => "null"
      case _ => defaultRoot.toString
    }))
}

class ImmutableEngine2[P1, P2, R](val defaultResult: R, val constraints: List[Constraint2[P1, P2, R]]) extends Engine2[P1, P2, R] {
  type CR = ImmutableEngine2[P1, P2, R]
  val root: RorN = buildFromConstraints(makeDefaultRoot(defaultResult), constraints)
  def addConstraint(c: C): CR = this
}

class MutableEngine2[P1, P2, R](val defaultValue: R) extends MutableEngine[R](defaultValue) with Engine2[P1, P2, R] 
