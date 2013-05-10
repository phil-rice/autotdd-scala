package org.autoTdd.helloScala

import org.autoTdd.helloScala.tests.IfThenParser
import org.scalatest.FlatSpec
import org.scalatest.matchers.ShouldMatchers
import org.autoTdd.helloScala.tests.NodeComparator
import org.autoTdd.helloScala.engine.Engine1Types


class NodeComparatorTest extends FlatSpec with ShouldMatchers with IfThenParserTestTrait {

  "A node comparator" should "return empty list if the node/results are both identical results" in {
    val x1 = p("x");
    val x2 = p("x");
    val actual = comparator.compare(x1, x2)
    assert(actual == List())
  }
  it should "return 'result X Y' if the node/results are different results" in {
    val x1 = p("x");
    val x2 = p("y");
    val actual = comparator.compare(x1, x2)
    assert(actual == List("result X Y"), actual)
  }

  it should "return empty list if the trees are both identical" in {
    val x1 = p("if a then x else y");
    val x2 = p("if a then x else y");
    val actual = comparator.compare(x1, x2)
    assert(actual == List())
  }

  it should "return 'because A B' if the trees have different becauses" in {
    val x1 = p("if a then x else y");
    val x2 = p("if b then x else y");
    val actual = comparator.compare(x1, x2)
    assert(actual == List("because A B"), actual)
  }

  it should "return description if left is result and right is node " in {
    val x1 = p("x");
    val x2 = p("if a then x else y");
    val actual = comparator.compare(x1, x2)
    val expected = List("left is result X Right is tree " + x2.right.get)
    assert(actual == expected, actual)

  }

  it should "return description if left is a node and right is a result " in {
    val x1 = p("if a then x else y");
    val x2 = p("x");
    val actual = comparator.compare(x1, x2)
    val expected = List("left is tree " + x1.right.get + " Right is result X")
    assert(actual == expected, actual)
  }

  it should "report errors in yes branch" in {
    val x1 = p("if a if b then x else y else z");
    val x2 = p("if a if c then x else y else z");
    val actual = comparator.compare(x1, x2)
    val expected = List("yes/because B C")
    assert(actual == expected, actual)
  }
  
  it should "report errors in no branch" in {
    val x1 = p("if a then x else if b then y else z");
    val x2 = p("if a then x else if c then y else z");
    val actual = comparator.compare(x1, x2)
    val expected = List("no/because B C")
    assert(actual == expected, actual)

  }
}