package org.autoTdd.helloScala.katas

import org.autoTdd.helloScala.engine.MutableEngine
import org.junit.runner.RunWith
import org.autoTdd.helloScala.engine.AutoTddRunner

class Hello3 {

}
@RunWith(classOf[AutoTddRunner])
object Hello2 {

  val get = MutableEngine.engine2[List[Int], Int, Int](0);
  val makeFrame = MutableEngine.engine2[List[Int], Int, Frame]()

  get constraint (List(7, 10, 4, 3), 0, 7,
    (rolls, i) => rolls.apply(i),
    (rolls, i) => i < rolls.length)

  get constraint (List(7, 10, 4, 3), 4, 0, (x: List[Int], i: Int) => 0, (x: List[Int], i: Int) => true);

  makeFrame.constraint(List(7, 2, 5, 5, 3, 0, 10, 2, 4), 0, NormalFrame(7, 2), (rolls, i) => NormalFrame(get(rolls, i), get(rolls, i + 1)))

  makeFrame.constraint(List(7, 2, 5, 5, 3, 0, 10, 2, 4), 0,
    NormalFrame(7, 2),
    (rolls: List[Int], i: Int) => {
      get(rolls, i)
      val f: Int = get.constraint(rolls, 0, 7, (rolls, i) => rolls(i), (rolls, i) => i < rolls.length);
      val s: Int = get.constraint(rolls, 1, 2, (rolls, i) => rolls(i), (rolls, i) => i < rolls.length);
      NormalFrame(f, s)
    })

  makeFrame.constraint(List(7, 2, 5, 5, 3, 0, 10, 2, 4), 2, SpareFrame(5, 5, 3),
    (rolls, i) => SpareFrame(get(rolls, i), get(rolls, i + 1), rolls(i + 2)),
    (rolls, i) => get(rolls, i) + get(rolls, i + 1) == 10)

  makeFrame.constraint(List(7, 2, 5, 5, 3, 0, 10, 2, 4), 4, NormalFrame(3, 0),
    (rolls, i) => NormalFrame(get(rolls, i), get(rolls, i + 1)))

  makeFrame.constraint(List(7, 2, 5, 5, 3, 0, 10, 2, 4), 6, StrikeFrame(10, 2, 4),
    (rolls, i) => StrikeFrame(rolls(i), get(rolls, i + 1), get(rolls, i + 2)),
    (rolls, i) => get(rolls, i) == 10)

  makeFrame.assertion(List(7), 0, NormalFrame(7, 0))

  makeFrame.assertion(List(5, 5), 0, SpareFrame(5, 5, 0))

  makeFrame.assertion(List(10), 0, StrikeFrame(10, 0, 0))

  makeFrame.assertion(List(10, 2), 0, StrikeFrame(10, 2, 0))

  //    val makeFrames = new FunnyFoldingEngine[Int, Int, Frame]
  //    val x = 0;
  //    makeFrames.constraint(0, Array(7, 2, 5, 5, 3, 0, 10, 2, 4),
  //      (f, acc) => x, Map(
  //        0 -> NormalFrame(7, 2),
  //        1 -> SpareFrame(5, 5, 3),
  //        2 -> NormalFrame(3, 0),
  //        3 -> StrikeFrame(10, 2, 4),
  //        4 -> NormalFrame(2, 4)))
  //    makeFrames.constraint(0, Array(7, 2, 5, 5, 3, 0, 10, 2, 4),
  //      (f, acc) => acc, Map(
  //        0 -> NormalFrame(7, 2),
  //        1 -> SpareFrame(5, 5, 3),
  //        2 -> NormalFrame(3, 0),
  //        3 -> StrikeFrame(10, 2, 4),
  //        4 -> NormalFrame(2, 4)))

}