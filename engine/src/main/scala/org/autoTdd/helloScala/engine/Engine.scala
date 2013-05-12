package org.autoTdd.helloScala.engine

import scala.language.experimental.macros
import scala.reflect.macros.Context
import org.autotdd.constraints.Constraint
import org.autotdd.constraints.CodeFn
import org.autotdd.constraints.Because

class ConstraintBecauseException(msg: String) extends RuntimeException(msg)
class ConstraintResultException(msg: String) extends RuntimeException(msg)
class EngineResultException(msg: String) extends RuntimeException(msg)
class ConstraintConflictException(msg: String) extends RuntimeException(msg)

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
    cs.size match {
      case 0 => root;
      case 1 =>
        val c = cs.head; if (c.hasDefaultBecause) Left(c.code) else buildFromConstraintsRemainder(withConstraint(root, cs.head), cs.tail)
      case _ => buildFromConstraintsRemainder(withConstraint(root, cs.head), cs.tail)
    }
  }
  private def buildFromConstraintsRemainder(root: RorN, cs: List[C]): RorN = {
    cs.size match {
      case 0 => root;
      case _ => buildFromConstraintsRemainder(withConstraint(root, cs.head), cs.tail)
    }
  }

  private def withConstraint(r: RorN, c: C): RorN = {
    val fn = makeClosureForBecause(c.params)
    r match {
      case Left(l) => Right(makeLeaf(c, l))
      case Right(r) if r.yes.isLeft =>
        makeClosureForBecause(c.params)(r.because.because) match {
          case true => dealWithYesInLeaf(r, c)
          case false => Right(r.copy(no = withConstraint(r.no, c)));
        }
      case Right(r) =>
        makeClosureForBecause(c.params)(r.because.because) match {
          case true => Right(r.copy(yes = withConstraint(r.yes, c)));
          case false => Right(r.copy(no = withConstraint(r.no, c)));
        }
    }
  }

  private def dealWithYesInLeaf(leaf: N, c: C): RorN = {
    if (c.hasDefaultBecause)
      addConstraintToLeaf(leaf, c)
    else
      makeClosureForBecause(leaf.inputs)(c.because.because) match {
        case true => addConstraintToLeaf(leaf, c) //ok we have problem so wimping out
        case false => Right(leaf.copy(yes = withConstraint(leaf.yes, c)));
      }
  }
  private def addConstraintToLeaf(leaf: N, c: C): RorN = {
    if (leaf.yes.isRight)
      throw new IllegalStateException;
    if (!c.hasDefaultBecause)
      if (leaf.because.becauseString != c.because.becauseString)
        throw new ConstraintConflictException("Cannot differentiate between \nExisting:\n" + leaf + "\nand new constraint:\n" + c)
    if (leaf.yes.left.get.description != c.code.description)
      throw new ConstraintConflictException("Cannot differentiate between \nExisting:\n" + leaf + "\nand new constraint:\n" + c)
    return Right(leaf.copy(extraConstraints = c :: leaf.extraConstraints))
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

  private def validateConstraint(c: C) {
    if (!makeClosureForBecause(c.params).apply(c.because.because))
      throw new ConstraintBecauseException(c.because.becauseString + " is not true for " + c.params);
    val actualFromConstraint = c.actualValueFromParameters
    if (actualFromConstraint != c.expected)
      throw new ConstraintResultException("Wrong result for " + c.code.description + " for " + c.params + "\nActual: " + actualFromConstraint + "\nExpected: " + c.expected);
  }

  private def checkConstraint(c: C) {
    validateConstraint(c);
    val actualFromEngine = applyParam(c.params);
    if (actualFromEngine != c.expected)
      throw new EngineResultException("Wrong result for " + c.code.description + " for " + c.params + "\nActual: " + actualFromEngine + "\nExpected: " + c.expected);
  }

  def addConstraintWithChecking(c: C, addingClosure: (C) => CR, default: CR): CR = {
    try {
      validateConstraint(c)
      val result = addingClosure(c)
      if (!EngineTest.testing)
        checkConstraint(c)
      result
    } catch {
      case e: Throwable if EngineTest.testing =>
        EngineTest.exceptions = EngineTest.exceptions + (c -> e); default
      case e: Throwable => throw e
    }
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
  def oldEngine: E
  def addConstraint(c: C): CR = {
    addConstraintWithChecking(c, (c) => {
      val newConstraints = (c :: constraints).reverse; //could do better..
      val result = newEngine(defaultResult, newConstraints);
      result
    }, oldEngine)
  }
}

abstract class MutableEngine[R](val defaultResult: R) extends Engine[R] {
  type CR = R
  var root: RorN = makeDefaultRoot(defaultResult)
  var constraints = List[C]()

  def addConstraint(c: C): CR = {
    addConstraintWithChecking(c, (c) => {
      val l = c :: constraints.reverse
      val newConstraints = l.reverse
      constraints = newConstraints
      root = buildFromConstraints(makeDefaultRoot(defaultResult), constraints);
      c.expected
    }, c.expected);
  }
}

