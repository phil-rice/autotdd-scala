package org.autoTdd.helloScala

import org.autoTdd.helloScala.engine._
import org.scalatest.FlatSpec
import org.scalatest.matchers.ShouldMatchers
import org.autoTdd.helloScala.engine.MutableEngine
import org.autoTdd.helloScala.engine.Constraint1

class Engine1Test extends FlatSpec with ShouldMatchers {

  "An empty Engine" should "return the default value" in {
    val engine_1 = MutableEngine.engine1[Int, Int](default = 1);
    assert(1 == engine_1(234))

    val engine_2 = MutableEngine.engine1[Int, Int](default = 2);
    assert(2 == engine_2(123))
  }

  "An engine with one constraint" should "apply that constraint or return the default" in {
    val engine = MutableEngine.engine1[Int, String](default = "Negative");
    val result = (p) => "P" + p
    val because = (x: Int) => x >= 0
    val expected = "P1"
    val actual: Any = engine.constraint(1, expected, result, because);
    assert(engine(-1) == "Negative")
    assert(actual == "P1")
    assert(engine(0) == "P0")
    assert(engine(1) == "P1")
    assert(engine(100) == "P100")
    assert(engine(-1) == "Negative")
    assert(engine(-100) == "Negative")
  }

  "A constraint without a result" should "return expected value when it applies" in {
    val engine = MutableEngine.engine1[Int, String](default = "Negative");
    val actual: String = engine.constraintBecause(1, "Positive", (x) => x >= 0);
    assert(actual == "Positive")
    assert(engine(0) == "Positive")
    assert(engine(1) == "Positive")
    assert(engine(100) == "Positive")
    assert(engine(-1) == "Negative")
    assert(engine(-100) == "Negative")
  }

  "An engine" should "throw ConstraintResultException if the expected value is not returned from the result function" in {
    val engine = MutableEngine.engine1[Int, String](default = "Negative");
    evaluating { val x: String = engine.constraint(1, "PX", (p) => "P" + p, (x) => x >= 0) } should produce[ConstraintResultException]
  }

  it should "throw ConstraintBecauseException if the because function is not true" in {
    val engine = MutableEngine.engine1[Int, String](default = "Negative");
    evaluating { val x: String = engine.constraint(1, "P1", (p) => "P" + p, (x) => x < 0) } should produce[ConstraintBecauseException]
  }

  "A constraint " should "have a becauseString that is the AST of the because parameter serialized" in {
    val engine: MutableEngine1[Int, String] = MutableEngine.engine1[Int, String](default = "Negative").asInstanceOf[MutableEngine1[Int, String]];
    engine.constraint(1, "P1", (p) => "P" + p, (x) => x >= 0)
    assert(engine.constraints.size == 1)
    val c = engine.constraints.head
    assert(c.becauseString == "((x: Int) => x.>=(0))", c.becauseString) //Note I don't know why I have an extra () and a '.' but I'm not complaining 
  }

  //TODO Not sure what to do about this. Ideally I would only do this if the because was identical, but it's hard to do identical functions
  //this crap behaviour is a place holder until I decide what to do about it
  //idea: ORs would be suitable so I could idea the idea of an OR constraint
  it should "ignore constraints if the result is already derived " in {
    val engine = MutableEngine.engine1[Int, String](default = "Negative");
    engine.constraintBecause(1, "Positive", (x) => x >= 0);
    engine.constraintBecause(2, "Positive", (x) => x >= 0);
    assert(engine(1) == "Positive")
    assert(engine(2) == "Positive")
    assert(engine(-1) == "Negative")
  }

  val pos = Constraint1[Int, String](1, "Pos", code = (x) => "Pos", codeString = "Pos", because = (x) => x > 0, becauseString = "+ve");
  val bigPos = Constraint1[Int, String](10, "BigPos", code = (x) => "BigPos", codeString = "BigPos", because = (x) => x > 5, becauseString = "v+ve");
  val vBigPos = Constraint1[Int, String](100, "VBigPos", code = (x) => "VBigPos", codeString = "VBigPos", because = (x) => x > 50, becauseString = "vv+ve");

  val neg = Constraint1[Int, String](-1, "Neg", code = (x) => "Neg", codeString = "Neg", because = (x) => x < 0, becauseString = "-ve");
  val bigNeg = Constraint1[Int, String](-10, "BigNeg", code = (x) => "BigNeg", codeString = "BigNeg", because = (x) => x < -5, becauseString = "v-ve");
  val vBigNeg = Constraint1[Int, String](-100, "VBigNeg", code = (x) => "VBigNeg", codeString = "VBigNeg", because = (x) => x < -50, becauseString = "vv-ve");

  "An engine " should "apply four constraints, whatever the order, in this smoke test" in {
    makeAndCheck(pos, bigPos, neg, bigNeg);
    makeAndCheck(neg, bigNeg, pos, bigPos);
    makeAndCheck(bigPos, pos, bigNeg, neg);
    makeAndCheck(bigNeg, neg, bigPos, pos);
  }

  it should "apply constraints, whatever the order, in this smoke test" in {
    makeAndCheck(pos, bigPos, neg, bigNeg, vBigPos, vBigNeg);
    makeAndCheck(neg, bigNeg, pos, bigPos, vBigPos, vBigNeg);
    makeAndCheck(bigPos, pos, bigNeg, neg, vBigPos, vBigNeg);
    makeAndCheck(bigNeg, neg, bigPos, pos, vBigPos, vBigNeg);
    makeAndCheck(vBigPos, vBigNeg, bigNeg, neg, bigPos, pos);
  }

  it should "have a decent to string " in {
    makeAndCheckToString(
      "if(v+ve)\n" +
        " if(vv+ve)\n" +
        "  VBigPos\n" +
        " else\n" +
        "  BigPos\n" +
        "else\n" +
        " if(v-ve)\n" +
        "  if(vv-ve)\n" +
        "   VBigNeg\n" +
        "  else\n" +
        "   BigNeg\n" +
        " else\n" +
        "  if(-ve)\n" +
        "   Neg\n" +
        "  else\n" +
        "   if(+ve)\n" +
        "    Pos\n" +
        "   else\n" +
        "    Zero\n", vBigPos, vBigNeg, bigNeg, neg, bigPos, pos);
  }

  def makeAndCheckToString(expected: String, constraints: Constraint1[Int, String]*) = {
    val engine = MutableEngine.engine1[Int, String](default = "Zero");
    for (c <- constraints)
      engine.addConstraint(c);
    val actual = engine.toString
    assert(expected == actual, "Expected\n" + expected + "\nActual:\n" + actual)
  }

  def makeAndCheck(constraints: Constraint1[Int, String]*) = {
    val engine = MutableEngine.engine1[Int, String](default = "Zero");
    for (c <- constraints)
      engine.addConstraint(c);
    for (c <- constraints) {
      val p = c.param
      assert(c.expected == engine(p), "\nEngine:\n" + engine + "\nConstraint: " + c);
    }
  }
}