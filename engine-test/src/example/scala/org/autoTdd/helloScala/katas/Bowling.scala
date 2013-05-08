package org.autoTdd.helloScala.katas

import org.autoTdd.helloScala.engine.MutableEngine

object Hello2 extends App {

  val get = MutableEngine.engine2[Array[Int], Int, Int](0);
  val makeFrame = MutableEngine.engine2[Array[Int], Int, Frame]()

  get constraint (Array(7, 10, 4, 3), 0, 7,
    (rolls, i) => rolls.apply(i),
    (rolls, i) => i < rolls.length)

  get constraint (Array(7, 10, 4, 3), 4, 0, (x: Array[Int], i: Int) => 0, (x: Array[Int], i: Int) => true);

  makeFrame.constraint(Array(7, 2, 5, 5, 3, 0, 10, 2, 4), 0, NormalFrame(7, 2), (rolls, i) => NormalFrame(get(rolls, i), get(rolls, i + 1)))

  makeFrame.constraint(Array(7, 2, 5, 5, 3, 0, 10, 2, 4), 0,
    NormalFrame(7, 2),
    (rolls, i) => {
      get(rolls, i)
      val f = get.constraint(rolls, 0, 7, (rolls, i) => rolls(i), (rolls, i) => i < rolls.length);
      val s = get.constraint(rolls, 1, 7, (rolls, i) => rolls(i), (rolls, i) => i < rolls.length);
      NormalFrame(f, s)
    },
    (rolls, i) => get(rolls, i) + get(rolls, i + 1) == 10)

  makeFrame.constraint(Array(7, 2, 5, 5, 3, 0, 10, 2, 4), 2, SpareFrame(5, 5, 3),
    (rolls, i) => SpareFrame(get(rolls, i), get(rolls, i + 1), rolls(i + 2)),
    (rolls, i) => get(rolls, i) + get(rolls, i + 1) == 10)

  makeFrame.constraint(Array(7, 2, 5, 5, 3, 0, 10, 2, 4), 4, NormalFrame(3, 0),
    (rolls, i) => NormalFrame(get(rolls, i), get(rolls, i + 1)))

  makeFrame.constraint(Array(7, 2, 5, 5, 3, 0, 10, 2, 4), 6, StrikeFrame(10, 2, 4),
    (rolls, i) => StrikeFrame(rolls(i), get(rolls, i + 1), get(rolls, i + 2)),
    (rolls, i) => get(rolls, i) == 10)

  makeFrame.assertion(Array(7), 0, NormalFrame(7, 0))

  makeFrame.assertion(Array(5, 5), 0, SpareFrame(5, 5, 0))

  makeFrame.assertion(Array(10), 0, SpareFrame(10, 0, 0))

  makeFrame.assertion(Array(10, 2), 0, SpareFrame(10, 2, 0))

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

  override def main(args: Array[String]) {
    val x = (y: Int) => 1;
    print(x)
  }
}