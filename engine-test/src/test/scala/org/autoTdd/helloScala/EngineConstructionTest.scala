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

class EngineConstructionTest extends FlatSpec with ShouldMatchers with Engine1Types[String, String] {
  def comparator = NodeComparator.comparator1[String, String]
  implicit def string_to_because(s: String) = new Because[B]((x) => x contains s, s.toString())
  implicit def string_to_result(s: String) = new CodeFn[RFn]((x) => s, s.toString())

  def node(b: B, inputs: List[Any], yes: RorN, no: RorN) = new Node(b, inputs, List(), yes, no);
  def rightNode(b: Because[B], inputs: List[Any], yes: RorN, no: RorN) = Right(new Node(b, inputs, List(), yes, no));

  val p = IfThenParser.parser1[String, String](
    becauses = Map("a" -> "a", "b" -> "b", "c" -> "c"),
    inputs = Map("a" -> "a", "b" -> "b", "ab" -> "ab", "c" -> "c"),
    thens = Map("w" -> "W", "x" -> "X", "y" -> "Y", "z" -> "Z"))

  def check(engine: Engine1[String, String], expected: String) {
    val actual = comparator.compare(p(expected), engine.root)
    assert(actual == List(), actual)
  }

  "An engine" should "change from root to if then with one constraint" in {
    val engine = Engine1[String, String](default = "Z");
    engine.constraint("a", "X", because = "a");
    check(engine, "if a/a then x else z")
  }

  it should "add to else path if first constraints doesnt match second" in {
    val engine = Engine1[String, String](default = "Z");
    engine.constraint("a", "X", because = "a");
    engine.constraint("b", "Y", because = "b");
    check(engine, "if a/a then x else if b/b then y else z")
  }

  it should "add to then path if second constraint is valid in first" in {
    val engine = Engine1[String, String](default = "Z");
    engine.constraint("a", "X", because = "a");
    engine.constraint("ab", "Y", because = "b");
    check(engine, "if a/a if b/ab then y else x else z")
  }
  it should "add constraint as an assertion if the because string is identical" in {
    val engine = Engine1[String, String](default = "Z");
    engine.constraint("a", "X", because = "a");
    engine.constraint("aa", "X", because = "a");
    check(engine, "if a/a then x else if b/b then y else z")

  }

}