package org.autoTdd.helloScala.engine

import scala.language.experimental.macros
import scala.reflect.macros.Context
import scala.reflect.macros.Context

abstract class Constraint[B, RFn, R](val expected: R, val code: RFn, val codeString: String, val because: B, val becauseString: String) {
  def params: List[Any]
}
case class Constraint1[P, R](val param: P, override val expected: R, override val code: (P) => R, override val codeString: String, override val because: (P) => Boolean, override val becauseString: String)
  extends Constraint[(P) => Boolean, (P) => R, R](expected, code, codeString, because, becauseString) {
  override def params = List(param)
}
case class Constraint2[P1, P2, R](val p1: P1, val p2: P2, override val expected: R, override val code: (P1, P2) => R, override val codeString: String, override val because: (P1, P2) => Boolean, override val becauseString: String)
  extends Constraint[(P1, P2) => Boolean, (P1, P2) => R, R](expected, code, codeString, because, becauseString) {
  override def params = List(p1, p2)
}

case class RFnAndDesc[RFn](val rfn: RFn, val desc: String)

case class Node[B, RFn](val because: B, val becauseString: String, val inputs: List[Any], val yes: Either[RFnAndDesc[RFn], Node[B, RFn]], no: Either[RFnAndDesc[RFn], Node[B, RFn]])

trait EngineTypes[R] {
  type B
  type RFn
  type C <: Constraint[B, RFn, R]

  type BecauseClosure = (B) => Boolean
  type ResultClosure = (RFn) => R

  type N = Node[B, RFn]
  type OptN = Option[Node[B, RFn]]
  type RorN = Either[RFnAndDesc[RFn], Node[B, RFn]]
  def makeClosureForBecause(params: List[Any]): BecauseClosure
  def makeClosureForResult(params: List[Any]): ResultClosure
}

trait BuildEngine[R] extends EngineTypes[R] {

  def buildFromConstraints(root: RorN, cs: List[C]): RorN = {
    if (cs.isEmpty)
      root
    else
      buildFromConstraints(withConstraint(root, cs.head), cs.tail)
  }

  private def withConstraint(r: RorN, c: C): RorN = {
    val fn = makeClosureForBecause(c.params)
    r match {
      case Left(l) => Right(makeLeaf(c, l))
      case Right(r) =>
        makeClosureForBecause(c.params).apply(r.because) match {
          case true => Right(r.copy(yes = withConstraint(r.yes, c)));
          case false => Right(r.copy(no = withConstraint(r.no, c)));
        }
    }
  }

  def makeLeaf(c: C, defaultResult: RFnAndDesc[RFn]): N = {
    val yes = Left(RFnAndDesc[RFn](c.code, c.codeString))
    val no = Left(defaultResult)
    Node[B, RFn](c.because, c.becauseString, c.params, yes, no)
  }

  private def findLastMatch(fn: BecauseClosure, root: OptN, lastMatch: OptN, params: List[Any]): OptN = {
    root match {
      case None => None
      case Some(r) =>
        fn(r.because) match {
          case true => findLastMatch(fn, r.yes, root, params)
          case false => findLastMatch(fn, r.no, lastMatch, params)
        }
    }
  }
  private def findLastMatch(fn: BecauseClosure, root: RorN, lastMatch: OptN, params: List[Any]): OptN = {
    root match {
      case Left(r) => lastMatch
      case Right(n) => findLastMatch(fn, Some(n), lastMatch, params)
    }
  }
}

trait EvaluateEngine[R] extends EngineTypes[R] {

  def evaluate(fn: BecauseClosure, n: RorN): RFn = {
    n match {
      case Left(r) => r.rfn
      case Right(n) => evaluate(fn, n)
    }
  }

  private def evaluate(fn: BecauseClosure, n: N): RFn = {
    fn(n.because) match {
      case false => evaluate(fn, n.no);
      case true => evaluate(fn, n.yes);
    }
  }
}

trait AddConstraints[R] extends EngineTypes[R] {
  type CR
  def addConstraint(c: C): CR
  def validateConstraint(c: C) {
    if (!makeClosureForBecause(c.params).apply(c.because))
      throw new ConstraintBecauseException(c.becauseString + " is not true for " + c.params);
    val actual = makeClosureForResult(c.params).apply(c.code)
    if (actual != c.expected)
      throw new ConstraintResultException("Wrong result for " + c.codeString + " for " + c.params + "\nActual: " + actual + "\nExpected: " + c.expected);
  }
}

trait Engine[R] extends BuildEngine[R] with AddConstraints[R] with EvaluateEngine[R] {
  def root: RorN
  def constraints: List[C]
  def makeDefaultRoot(defaultRoot: R): RorN
  def realConstraint(c: C): C = c
}

trait EngineToString[R] extends EngineTypes[R] {
  def root: RorN
  def toString(indent: String, root: RorN): String = {
    root match {
      case Left(rfnAndDesc) => indent + rfnAndDesc.desc + "\n"
      case Right(node) =>
        indent + "if(" + node.becauseString + ")\n" +
          toString(indent + " ", node.yes) +
          indent + "else\n" +
          toString(indent + " ", node.no)
    }
  }
  override def toString: String = toString("", root)
}

trait ImmutableEngine[R] extends Engine[R] {
  type E
  type CR = E
  def defaultResult: R

  def newEngine(defaultResult: R, constraints: List[C]): E

  def addConstraint(c: C): CR = {
    validateConstraint(c)
    val newConstraints = (c :: constraints).reverse //could do better..
    newEngine(defaultResult, newConstraints)
  }
}

object Engine1 {

  def blankTrue[P, R](r: R): (P) => Boolean = (p) => true;
  def blankR[P, R](r: R): (P) => R = (p) => r;

  def constraintImplAssertion[P: c.WeakTypeTag, R: c.WeakTypeTag, CR: c.WeakTypeTag](c: Context)(p: c.Expr[P], expected: c.Expr[R]): c.Expr[CR] = {
    import c.universe._
    val expr = reify { (c.Expr[Engine1[P, R]](c.prefix.tree)).splice.constraintImplAssertion(p.splice, expected.splice) }
    c.Expr[CR](expr.tree)
  }
  def constraintImplNoCode[P: c.WeakTypeTag, R: c.WeakTypeTag, CR: c.WeakTypeTag](c: Context)(p: c.Expr[P], expected: c.Expr[R], because: c.Expr[(P) => Boolean]): c.Expr[CR] = {
    import c.universe._
    val expr = reify { (c.Expr[Engine1[P, R]](c.prefix.tree)).splice.constraintImplNoCode(p.splice, expected.splice, because.splice, c.literal(show(because.tree)).splice) }
    val result = c.Expr[CR](expr.tree)
    result
  }
  def constraintImplNoBecause[P: c.WeakTypeTag, R: c.WeakTypeTag, CR: c.WeakTypeTag](c: Context)(p: c.Expr[P], expected: c.Expr[R], code: c.Expr[(P) => R]): c.Expr[CR] = {
    import c.universe._
    val expr = reify { (c.Expr[Engine1[P, R]](c.prefix.tree)).splice.constraintImplNoBecause(p.splice, expected.splice, code.splice, c.literal(show(code.tree)).splice) }
    c.Expr[CR](expr.tree)
  }
  def constraintImplFull[P: c.WeakTypeTag, R: c.WeakTypeTag, CR: c.WeakTypeTag](c: Context)(p: c.Expr[P], expected: c.Expr[R], code: c.Expr[(P) => R] = null, because: c.Expr[(P) => Boolean] = null): c.Expr[CR] = {
    import c.universe._
    val expr = reify { (c.Expr[Engine1[P, R]](c.prefix.tree)).splice.constraintImplFull(p.splice, expected.splice, code.splice, c.literal(show(code.tree)).splice, because.splice, c.literal(show(because.tree)).splice) }
    c.Expr[CR](expr.tree)
  }
}

trait Engine1[P, R] extends Engine[R] with Function1[P, R] with EngineToString[R] {
  type B = (P) => Boolean
  type RFn = (P) => R
  type C = Constraint1[P, R]

  def apply(p: P): R = evaluate(b => b(p), root)(p)
  def assertion(p: P, expected: R): CR = macro Engine1.constraintImplAssertion[P, R, CR]
  def constraintBecause(p: P, expected: R, because: B): CR = macro Engine1.constraintImplNoCode[P, R, CR]
  def constraint(p: P, expected: R, code: (P) => R): CR = macro Engine1.constraintImplNoBecause[P, R, CR]
  def constraint(p: P, expected: R, code: (P) => R, because: (P => Boolean)): CR = macro Engine1.constraintImplFull[P, R, CR]

  def constraintImplAssertion(p: P, expected: R): CR = {
    addConstraint(realConstraint(Constraint1(p, expected, (P) => expected, expected.toString, (P) => true, "true")))
  }

  def constraintImplNoCode(p: P, expected: R, because: B, becauseString: String): CR =
    addConstraint(realConstraint(Constraint1(p, expected, (P) => expected, expected.toString, because, becauseString)))

  def constraintImplNoBecause(p: P, expected: R, code: RFn, codeString: String): CR =
    addConstraint(realConstraint(Constraint1(p, expected, code, codeString, (P) => true, "true")))

  def constraintImplFull(p: P, expected: R, code: RFn = null, codeString: String = "", because: B = null, becauseString: String = ""): CR =
    addConstraint(realConstraint(Constraint1(p, expected, code, codeString, because, becauseString)))

  def makeClosureForBecause(params: List[Any]) = (b) => b(params(0).asInstanceOf[P])
  def makeClosureForResult(params: List[Any]) = (r) => r(params(0).asInstanceOf[P])

  def makeDefaultRoot(defaultRoot: R): RorN = Left(RFnAndDesc[RFn]((p: P) => defaultRoot, defaultRoot.toString))
}

object Engine2 {
  def constraintImplAssertion[P1: c.WeakTypeTag, P2: c.WeakTypeTag, R: c.WeakTypeTag, CR: c.WeakTypeTag](c: Context)(p1: c.Expr[P1], p2: c.Expr[P2], expected: c.Expr[R]): c.Expr[CR] = {
    import c.universe._
    val expr = reify { (c.Expr[Engine2[P1, P2, R]](c.prefix.tree)).splice.constraintImplAssertion(p1.splice, p2.splice, expected.splice) }
    c.Expr[CR](expr.tree)
  }
  def constraintImplNoCode[P1: c.WeakTypeTag, P2: c.WeakTypeTag, R: c.WeakTypeTag, CR: c.WeakTypeTag](c: Context)(p1: c.Expr[P1], p2: c.Expr[P2], expected: c.Expr[R], because: c.Expr[(P1, P2) => Boolean]): c.Expr[CR] = {
    import c.universe._
    val expr = reify { (c.Expr[Engine2[P1, P2, R]](c.prefix.tree)).splice.constraintImplNoCode(p1.splice, p2.splice, expected.splice, because.splice, c.literal(show(because.tree)).splice) }
    c.Expr[CR](expr.tree)
  }
  def constraintImplNoBecause[P1: c.WeakTypeTag, P2: c.WeakTypeTag, R: c.WeakTypeTag, CR: c.WeakTypeTag](c: Context)(p1: c.Expr[P1], p2: c.Expr[P2], expected: c.Expr[R], code: c.Expr[(P1, P2) => R]): c.Expr[CR] = {
    import c.universe._
    val expr = reify { (c.Expr[Engine2[P1, P2, R]](c.prefix.tree)).splice.constraintImplNoBecause(p1.splice, p2.splice, expected.splice, code.splice, c.literal(show(code.tree)).splice) }
    c.Expr[CR](expr.tree)
  }
  def constraintImplFull[P1: c.WeakTypeTag, P2: c.WeakTypeTag, R: c.WeakTypeTag, CR: c.WeakTypeTag](c: Context)(p1: c.Expr[P1], p2: c.Expr[P2], expected: c.Expr[R], code: c.Expr[(P1, P2) => R], because: c.Expr[(P1, P2) => Boolean]): c.Expr[CR] = {
    import c.universe._
    val expr = reify { (c.Expr[Engine2[P1, P2, R]](c.prefix.tree)).splice.constraintImplFull(p1.splice, p2.splice, expected.splice, code.splice, c.literal(show(code.tree)).splice, because.splice, c.literal(show(because.tree)).splice) }
    c.Expr[CR](expr.tree)
  }
}

trait Engine2[P1, P2, R] extends Engine[R] with Function2[P1, P2, R] with EngineToString[R] {
  type B = (P1, P2) => Boolean
  type RFn = (P1, P2) => R
  type C = Constraint2[P1, P2, R]

  def apply(p1: P1, p2: P2): R = {
    val rFn = evaluate(b => b(p1, p2), root)
    rFn(p1, p2)
  }

  def makeClosureForBecause(params: List[Any]) = (b) => b(params(0).asInstanceOf[P1], params(1).asInstanceOf[P2])
  def makeClosureForResult(params: List[Any]) = (r) => r(params(0).asInstanceOf[P1], params(1).asInstanceOf[P2])

  def assertion(p1: P1, p2: P2, expected: R): CR = macro Engine2.constraintImplAssertion[P1, P2, R, CR]
  def constraintBecause(p1: P1, p2: P2, expected: R, because: (P1, P2) => Boolean): CR = macro Engine2.constraintImplNoCode[P1, P2, R, CR]
  def constraint(p1: P1, p2: P2, expected: R, code: (P1, P2) => R): CR = macro Engine2.constraintImplNoBecause[P1, P2, R, CR]
  def constraint(p1: P1, p2: P2, expected: R, code: (P1, P2) => R, because: (P1, P2) => Boolean): CR = macro Engine2.constraintImplFull[P1, P2, R, CR]

  def constraintImplAssertion[CRDummy <: CR](p1: P1, p2: P2, expected: R): CRDummy =
    addConstraint(realConstraint(Constraint2(p1, p2, expected, (P1, P2) => expected, expected.toString, (P1, P2) => true, "true"))).asInstanceOf[CRDummy]
  def constraintImplNoCode[CRDummy <: CR](p1: P1, p2: P2, expected: R, because: B, becauseString: String): CRDummy =
    addConstraint(realConstraint(Constraint2(p1, p2, expected, (P1, P2) => expected, expected.toString, because, becauseString))).asInstanceOf[CRDummy]
  def constraintImplNoBecause[CRDummy <: CR](p1: P1, p2: P2, expected: R, code: RFn, codeString: String): CRDummy =
    addConstraint(realConstraint(Constraint2(p1, p2, expected, code, codeString, (P1, P2) => true, "true"))).asInstanceOf[CRDummy]
  def constraintImplFull[CRDummy <: CR](p1: P1, p2: P2, expected: R, code: RFn, codeString: String, because: B, becauseString: String): CRDummy =
    addConstraint(realConstraint(Constraint2(p1, p2, expected, code, codeString, because, becauseString))).asInstanceOf[CRDummy]

  def makeDefaultRoot(defaultRoot: R): RorN = Left(RFnAndDesc((p1, p2) => defaultRoot, defaultRoot.toString))
}

class ImmutableEngine1[P, R](val constraints: List[Constraint1[P, R]]) extends Engine1[P, R] {
  type CR = ImmutableEngine1[P, R]
  def root = null
  def addConstraint(c: Constraint1[P, R]) = this
}

class ImmutableEngine2[P1, P2, R](val defaultResult: R, val constraints: List[Constraint2[P1, P2, R]]) extends Engine2[P1, P2, R] {
  type CR = ImmutableEngine2[P1, P2, R]
  val root: RorN = buildFromConstraints(makeDefaultRoot(defaultResult), constraints)
  def addConstraint(c: C): CR = this
}

object MutableEngine {
  def engine1[P, R](default: R = null) = new MutableEngine1[P, R](default)
  def engine2[P1, P2, R](default: R = null) = new MutableEngine2[P1, P2, R](default)
}

abstract class MutableEngine[R](val defaultResult: R) extends Engine[R] {
  type CR = R
  var root: RorN = makeDefaultRoot(defaultResult)
  var constraints = List[C]()

  def addConstraint(c: C): CR = {
    validateConstraint(c)
    constraints = (c :: constraints).reverse //could do better..
    root = buildFromConstraints(makeDefaultRoot(defaultResult), constraints)
    c.expected
  }
}
class MutableEngine1[P, Result](val defaultValue: Result) extends MutableEngine[Result](defaultValue) with Engine1[P, Result] {

}

class MutableEngine2[P1, P2, Result](val defaultValue: Result) extends MutableEngine[Result](defaultValue) with Engine2[P1, P2, Result] {

}
