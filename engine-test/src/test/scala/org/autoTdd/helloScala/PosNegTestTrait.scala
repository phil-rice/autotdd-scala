package org.autoTdd.helloScala

import org.autoTdd.helloScala.engine.Constraint1

trait PosNegTestTrait {
 val pos = Constraint1[Int, String](1, "Pos", code = (x: Int) => "Pos", because = (x: Int) => x > 0);
  val bigPos = Constraint1[Int, String](10, "BigPos", code = (x: Int) => "BigPos", because = (x: Int) => x > 5);
  val vBigPos = Constraint1[Int, String](100, "VBigPos", code = (x: Int) => "VBigPos", because = (x: Int) => x > 50);

  val neg = Constraint1[Int, String](-1, "Neg", code = (x: Int) => "Neg", because = (x: Int) => x < 0);
  val bigNeg = Constraint1[Int, String](-10, "BigNeg", code = (x: Int) => "BigNeg", because = (x: Int) => x < -5);
  val vBigNeg = Constraint1[Int, String](-100, "VBigNeg", code = (x: Int) => "VBigNeg", because = (x: Int) => x < -50);
}