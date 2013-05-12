package org.autoTdd.helloScala

import org.scalatest.FlatSpec
import org.scalatest.matchers.ShouldMatchers
import org.autoTdd.helloScala.engine.Engine2
import org.autoTdd.helloScala.engine.Engine2

class EngineConstructionSmokeTest extends FlatSpec with ShouldMatchers with PosNegTestTrait {

  "The bowling kata get engine" should "build correctly" in {
    val get = Engine2[List[Int], Int, Int](0);

    get constraint (List(7, 10, 4, 3), 0, 7,
      (rolls: List[Int], i: Int) => rolls.apply(i),
      (rolls: List[Int], i: Int) => i >= 0 && i < rolls.length)

    get constraint (List(7, 10, 4, 3), -1, 0)
    get constraint (List(7, 10, 4, 3), 4, 0)
    get constraint (List(7, 10, 4, 3), 5, 0)
    assert(get.toString()
      ==
      "if(((rolls: List[Int], i: Int) => i.>=(0).&&(i.<(rolls.length))))\n" +
      " ((rolls: List[Int], i: Int) => rolls.apply(i))\n" +
      "else\n" +
      " 0\n", get.toString)
  }
}