package org.autoTdd.helloScala

import org.scalatest.FlatSpec

import org.scalatest.matchers.ShouldMatchers
import org.autoTdd.helloScala.engine.MutableEngine
import org.autoTdd.helloScala.engine.Engine1
import org.autoTdd.helloScala.tests.NodeComparator
import org.autoTdd.helloScala.tests.IfThenParser
import org.autotdd.constraints.CodeFn
import org.autotdd.constraints.Because
import org.autoTdd.helloScala.engine.Node
import org.autoTdd.helloScala.engine._

class EngineConstructionTest extends FlatSpec with ShouldMatchers with IfThenParserTestTrait {
  def check(engine: Engine1[String, String], expected: String) {
    val exceptedTree = p(expected)
    val actual = comparator.compare(exceptedTree, engine.root)
    assert(actual == List(), actual + "\nExpected: " + exceptedTree + "\n Actual: " + engine.root + "\nEngine:\n" + engine)
  }

  "An engine" should "change from root to if then with one constraint" in {
    val engine = Engine1[String, String](default = "Z");
    engine.constraint("A", "X", because = "A");
    check(engine, "if a/a then x else z")
    checkConstraints(engine, "A");
  }

  it should "add to else path if first constraints doesnt match second" in {
    val engine = Engine1[String, String](default = "Z");
    engine.constraint("A", "X", because = "A");
    engine.constraint("B", "Y", because = "B");
    check(engine, "if a/a then x else if b/b then y else z")
    checkConstraints(engine, "A", "B");
  }

  it should "add to then path if second constraint is valid in first" in {
    val engine = Engine1[String, String](default = "Z");
    engine.constraint("A", "X", because = "A");
    engine.constraint("AB", "Y", because = "B");
    check(engine, "if a/a if b/ab then y else x else z")
    checkConstraints(engine, "A", "B");
  }

  it should "add constraint as an assertion if the because string is identical" in {
    val engine = Engine1[String, String](default = "Z");
    engine.constraint("AB", "X", because = "A");
    engine.constraint("AA", "X", because = "A");
    check(engine, "if a/ab#a/aa->x then x else  z")
    checkConstraints(engine, "A", "A");
  }

  it should "keep the order of constraints" in {
    val engine = Engine1[String, String](default = "Z");
    engine.constraint("A", "X", because = "A");
    engine.constraint("C", "X", because = "C");
    engine.constraint("B", "X", because = "B");
    checkConstraints(engine, "A", "C", "B");
  }

  //TODO Consider how to deal with identical result, different because. It's not clear to me what I should do
  it should "throw exception if  cannot differentiate inputs, identical result, different because" in {
    val engine = Engine1[String, String](default = "Z");
    engine.constraint("AB", "X", because = "B");
    evaluating { engine.constraint("AB", "X", because = "A") } should produce[ConstraintConflictException]
  }
  it should "throw exception if  cannot differentiate inputs, different result" in {
    val engine = Engine1[String, String](default = "Z");
    engine.constraint("AB", "Y", because = "B");
    evaluating { engine.constraint("AB", "X", because = "A") } should produce[ConstraintConflictException]
  }

  it should "Replace default if first because isn't specified" in {
    val engine = Engine1[String, String](default = "X");
    engine.constraint("A", "X")
    check(engine, "x")
  }

  it should "Throw ConstraintConflictException  if first constraint is assertion and comes to wrong result" in {
    val engine = Engine1[String, String](default = "Z");
    engine.constraint("A", "X")
    check(engine, "x")
  }

  it should "assertions should add themselves to existing nodes" in {
    val engine = Engine1[String, String](default = "Z");
    engine.constraint("A", "X", because = "A")
    check(engine, "if a/a then x else z") //just documenting what we have 
    engine.constraint("AA", "X")
    val aAsConstraint = engine.constraints(0)
    val aaAsConstraint = engine.constraints(1)
    val expected: RorN = Right(Node[B, RFn, String, C](aAsConstraint.because, aAsConstraint.params, List(aaAsConstraint), Left("X"), Left("Z")))
    assertMatches(engine.root, expected)
  }

  it should "throw ConstraintConflictException if the added constraint is an assertion and comes to wrong result" in {
    val engine = Engine1[String, String](default = "Z");
    engine.constraint("A", "X", because = "A")
    evaluating { engine.constraint("AA", "X") } should produce[ConstraintConflictException]
  }

  def checkConstraints(engine: Engine1[String, String], expected: String*) {
    assert(engine.constraints.size == expected.size)
    for ((c, a) <- (engine.constraints, expected).zipped) {
      assert(c.because.becauseString == a, "Expected: " + a + " Actual " + c + "\n   Constraints: " + engine.constraints)
    }
  }

}