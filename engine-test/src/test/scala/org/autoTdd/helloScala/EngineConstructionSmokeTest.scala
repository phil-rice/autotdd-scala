package org.autoTdd.helloScala

import org.scalatest.FlatSpec
import org.scalatest.matchers.ShouldMatchers
import org.autoTdd.helloScala.engine.Engine2
import org.autoTdd.helloScala.engine.Engine2
import org.autoTdd.helloScala.katas.Frame
import org.autoTdd.helloScala.katas.NormalFrame
import org.autoTdd.helloScala.katas.SpareFrame
import org.autoTdd.helloScala.katas.StrikeFrame

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

  "The bowling kata makeFrame engine" should "build correctly" in {
    val get = Engine2[List[Int], Int, Int](0);
    get constraint (List(7, 10, 4, 3), 0, 7,
      (rolls: List[Int], i: Int) => rolls.apply(i),
      (rolls: List[Int], i: Int) => i >= 0 && i < rolls.length)

    val makeFrame = Engine2[List[Int], Int, Frame]()

    makeFrame.constraint(List(7, 2, 5, 5, 3, 0, 10, 2, 4), 0, NormalFrame(7, 2),
      (rolls: List[Int], i: Int) => NormalFrame(get(rolls, i), get(rolls, i + 1)),
      because = (rolls: List[Int], i: Int) => true)

    makeFrame.constraint(List(7, 2, 5, 5, 3, 0, 10, 2, 4), 0,
      NormalFrame(7, 2),
      (rolls: List[Int], i: Int) => {
        NormalFrame(get(rolls, i), get(rolls, i + 1))
      })

    makeFrame.constraint(List(7, 2, 5, 5, 3, 0, 10, 2, 4), 6, StrikeFrame(10, 2, 4),
      (rolls: List[Int], i: Int) => StrikeFrame(rolls(i), get(rolls, i + 1), get(rolls, i + 2)),
      because = (rolls: List[Int], i: Int) => get(rolls, i) == 10)

    makeFrame.constraint(List(7, 2, 5, 5, 3, 0, 10, 2, 4), 2, SpareFrame(5, 5, 3),
      (rolls: List[Int], i: Int) => SpareFrame(get(rolls, i), get(rolls, i + 1), get(rolls, i + 2)),
      because = (rolls: List[Int], i: Int) => get(rolls, i) + get(rolls, i + 1) == 10)

    makeFrame.constraint(List(7, 2, 5, 5, 3, 0, 10, 2, 4), 4, NormalFrame(3, 0),
      (rolls: List[Int], i: Int) => NormalFrame(get(rolls, i), get(rolls, i + 1)))

    makeFrame.assertion(List(7), 0, NormalFrame(7, 0))

    makeFrame.assertion(List(5, 5), 0, SpareFrame(5, 5, 0))

    makeFrame.assertion(List(10), 0, StrikeFrame(10, 0, 0))

    makeFrame.assertion(List(10, 2), 0, StrikeFrame(10, 2, 0))
  }

}