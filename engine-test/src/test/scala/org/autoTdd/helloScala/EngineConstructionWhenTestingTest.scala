package org.autoTdd.helloScala

import org.scalatest.FlatSpec
import org.scalatest.matchers.ShouldMatchers
import org.autoTdd.helloScala.engine.EngineTest
import org.autoTdd.helloScala.engine.Engine1
import org.autoTdd.helloScala.engine.EngineTest

class EngineConstructionWhenTestingTest extends FlatSpec with ShouldMatchers with IfThenParserTestTrait {

  "An Engine" should "Remember exceptions instead of adding them, when in test mode" in {
    val engine = Engine1[String, String](default = "Z");
    engine.constraint("AB", "X", because = "B");
    //    assert (List() == engine.exceptionsWhileBuildingForTesting)
    engine.constraint("AB", "X", because = "A")

  }
  "An Engine" should "Throw an exception when accessed if it failed to build properly" in {

  }
}