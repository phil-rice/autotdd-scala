package org.autoTdd.helloScala

import org.autoTdd.helloScala.engine.ConstraintConflictException
import org.autoTdd.helloScala.engine.Engine1
import org.autoTdd.helloScala.engine.EngineTest
import org.autoTdd.helloScala.engine.EngineTest
import org.scalatest.FlatSpec
import org.scalatest.matchers.ShouldMatchers

class EngineConstructionWhenTestingTest extends FlatSpec with ShouldMatchers with IfThenParserTestTrait {

  "An Engine" should "Remember exceptions instead of adding them, when in test mode" in {
    val engine = Engine1[String, String](default = "Z");
    EngineTest.test(() => {
      engine.constraint("AB", "X", because = "B");
      engine.constraint("AB", "X", because = "A")
    })
    val ab_a = engine.constraints(1)
    assert(1 == EngineTest.exceptions.size)
    assert(EngineTest.exceptions(ab_a).isInstanceOf[ConstraintConflictException]);
  }
//  "An Engine" should "Throw an exception when accessed if it failed to build properly" in {
//    val engine = Engine1[String, String](default = "Z");
//    EngineTest.test(() => {
//      engine.constraint("AB", "X", because = "B");
//      engine.constraint("AB", "X", because = "A")
//    })
//    evaluating { engine("A") } should produce[CannotAccessBrokenEngineException]
//  }
}