package org.autoTdd.helloScala

import org.scalatest.FlatSpec
import org.scalatest.matchers.ShouldMatchers
import org.autoTdd.helloScala.engine.Engine1
import org.autoTdd.helloScala.engine.AssertionException
import org.autoTdd.helloScala.engine.ConstraintConflictException

class EngineSecondConstraintTests extends FlatSpec with ShouldMatchers with EngineTests {

  "An  engine" should "add to yes clause if because is true for root" in {
    val engine = Engine1[String, String](default = "Z");
    engine.constraint("A", "X", because = "A");
    engine.constraint("AB", "Y", because = "B");
    check(engine, "if a/a if b/ab then y else x  else z")
    checkConstraints(engine, "A", "B");
  }

  "An  engine" should "add to no clause if because is false for root" in {
    val engine = Engine1[String, String](default = "Z");
    engine.constraint("A", "X", because = "A");
    engine.constraint("B", "Y", because = "B");
    check(engine, "if a/a then x else if b/b then y else z")
    checkConstraints(engine, "A", "B");
  }

  it should "Add assertions to the yes if constraint comes to correct value" in {
    val engine = Engine1[String, String](default = "Z");
    engine.constraint("A", "X", because = "A")
    engine.constraint("AB", "X");
    check(engine, "if a/a then x else z")
  }

  it should "Add assertions to the no if constraint comes to correct value" in {
    val engine = Engine1[String, String](default = "Z");
    engine.constraint("A", "X", because = "A")
    engine.constraint("B", "Z");
    check(engine, "if a/a then x else z")
  }
  
  //TODO Consider how to deal with identical result, different because. It's not clear to me what I should do
  it should "throw exception if  cannot differentiate inputs, identical result, different because" in {
    val engine = Engine1[String, String](default = "Z");
    engine.constraint("AB", "X", because = "A");
    evaluating { engine.constraint("AB", "X", because = "B") } should produce[ConstraintConflictException]
  }
  
  it should "Throw AssertionException if constraint matches root condition and comes to wrong conclusion" in {
    val engine = Engine1[String, String](default = "Z");
    engine.constraint("A", "X", because = "A")
    evaluating { engine.constraint("AB", "Z") } should produce[AssertionException]
  }
  it should "Throw AssertionException if constraint doesnt match root condition and comes to wrong conclusion" in {
    val engine = Engine1[String, String](default = "Z");
    engine.constraint("A", "X", because = "A")
    evaluating { engine.constraint("B", "X") } should produce[AssertionException]
  }
}