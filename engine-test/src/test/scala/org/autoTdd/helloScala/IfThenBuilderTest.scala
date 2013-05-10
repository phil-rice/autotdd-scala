package org.autoTdd.helloScala

import scala.util.Either
import scala.util.Left
import org.autoTdd.helloScala.engine.Node
import org.autoTdd.helloScala.tests.IfThenParser
import org.autotdd.constraints.CodeFn
import org.scalatest.FlatSpec
import org.scalatest.matchers.ShouldMatchers
import org.autotdd.constraints.Because

class IfThenBuilderTest extends FlatSpec with ShouldMatchers {
  val p = new IfThenParser[String, String](
    becauses = Map("a" -> "A", "b" -> "B", "c" -> "C"),
    inputs = Map("a" -> "I1", "b" -> "I2", "c" -> "I3"),
    thens = Map("w" -> "W", "x" -> "X", "y" -> "Y", "z" -> "Z"))

  def left(s: String): Either[CodeFn[String], Node[String, String]] =
    Left(CodeFn[String](s, s));
  val becauseA = Because[String]("A", "A")
  val becauseB = Because[String]("B", "B")
  "An If Then Builder " should "parse a simple If then else statement, substituting Becauses and Thens" in {
    val n = p("if a then x else y")
    val expected = Node[String, String](becauseA, List(), left("X"), left("Y"))
    assert(expected == n, n)
  }

  it should "parse an If then else statement with inputs" in {
    val n = p("if a/a,b then x else y")
    val expected = Node[String, String](becauseA, List("I1", "I2"), left("X"), left("Y"))
    assert(expected == n, n)
  }

  it should "parse nested if then else statements " in {
    val n = p("if a/a,b if b then w else x else y")
    val expected = Node[String, String](becauseA, List("I1", "I2"), Right(Node[String, String](becauseB, List(), left("W"), left("X"))), left("Y"))
    assert(expected == n, n)
  }
}