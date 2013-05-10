package org.autoTdd.helloScala.engine

import scala.language.experimental.macros
import scala.reflect.macros.Context
import org.autotdd.constraints.Constraint
import org.autotdd.constraints.CodeFn
import org.autotdd.constraints.Because

class ConstraintBecauseException(msg: String) extends RuntimeException(msg)
class ConstraintResultException(msg: String) extends RuntimeException(msg)
class EngineResultException(msg: String) extends RuntimeException(msg)

case class Node[B, RFn, R, C <: Constraint[B, RFn, R]](val because: Because[B], val inputs: List[Any], val extraConstraints: List[C], val yes: Either[CodeFn[RFn], Node[B, RFn, R, C]], no: Either[CodeFn[RFn], Node[B, RFn, R, C]])

trait EngineTypes[R] {
  type B
  type RFn
  type C <: Constraint[B, RFn, R]

  type BecauseClosure = (B) => Boolean
  type ResultClosure = (RFn) => R

  type N = Node[B, RFn, R, C]
  type Code = CodeFn[RFn]
  type OptN = Option[N]
  type RorN = Either[Code, N]
}

trait BuildEngine[R] extends EngineTypes[R] {

  def makeClosureForBecause(params: List[Any]): BecauseClosure
  def makeClosureForResult(params: List[Any]): ResultClosure

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
        makeClosureForBecause(c.params)(r.because.because) match {
          case true => Right(r.copy(yes = withConstraint(r.yes, c)));
          case false => Right(r.copy(no = withConstraint(r.no, c)));
        }
    }
  }

  def makeLeaf(c: C, defaultResult: CodeFn[RFn]): N = {
    val yes = Left(c.code)
    val no = Left(defaultResult)
    Node(c.because, c.params, List(), yes, no)
  }

  private def findLastMatch(fn: BecauseClosure, root: OptN, lastMatch: OptN, params: List[Any]): OptN = {
    root match {
      case None => None
      case Some(r) =>
        fn(r.because.because) match {
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
    fn(n.because.because) match {
      case false => evaluate(fn, n.no);
      case true => evaluate(fn, n.yes);
    }
  }
}

trait AddConstraints[R] extends EngineTypes[R] {
  type CR
  def addConstraint(c: C): CR
}

trait Engine[R] extends BuildEngine[R] with AddConstraints[R] with EvaluateEngine[R] {
  def root: RorN
  def constraints: List[C]
  def makeDefaultRoot(defaultRoot: R): RorN
  def realConstraint(c: C): C = c

  def validateConstraint(c: C) {
    if (!makeClosureForBecause(c.params).apply(c.because.because))
      throw new ConstraintBecauseException(c.because.becauseString + " is not true for " + c.params);
    val actualFromConstraint = makeClosureForResult(c.params).apply(c.code.rfn)
    if (actualFromConstraint != c.expected)
      throw new ConstraintResultException("Wrong result for " + c.code.description + " for " + c.params + "\nActual: " + actualFromConstraint + "\nExpected: " + c.expected);
  }

  def checkConstraint(c: C) {
    validateConstraint(c);
    val actualFromEngine = applyParam(c.params);
    if (actualFromEngine != c.expected)
      throw new EngineResultException("Wrong result for " + c.code.description + " for " + c.params + "\nActual: " + actualFromEngine + "\nExpected: " + c.expected);
  }

  def applyParam(params: List[Any]): R = {
    val rfn = evaluate(makeClosureForBecause(params), root)
    makeClosureForResult(params)(rfn)
  }

}

trait EngineToString[R] extends EngineTypes[R] {
  def root: RorN
  def toString(indent: String, root: RorN): String = {
    root match {
      case Left(result) => indent + result.description + "\n"
      case Right(node) =>
        indent + "if(" + node.because.becauseString + ")\n" +
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
    val result = newEngine(defaultResult, newConstraints)
    checkConstraint(c)
    result
  }
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

