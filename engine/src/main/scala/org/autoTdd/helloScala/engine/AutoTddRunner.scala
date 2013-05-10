package org.autoTdd.helloScala.engine

import org.junit.runner.Description
import scala.collection.JavaConversions._
import org.junit.runner.Runner
import org.junit.runner.notification.RunNotifier
import org.autoTdd.helloScala.engine._
import java.util.Arrays
import java.lang.reflect.Modifier
import scala.reflect.runtime.{ universe => ru }
import scala.reflect.ClassTag
import reflect.runtime.universe._
import java.lang.reflect.Method
import org.junit.runner.Result
import junit.framework.Assert
import org.junit.runner.notification.Failure
import org.autotdd.constraints.Constraint

class AutoTddRunner(val clazz: Class[Any]) extends Runner {

  val getDescription = Description.createSuiteDescription("ATDD: " + clazz.getName);
  val instance = instantiate(clazz)

  var engineMap: Map[Description, Engine[Any]] = Map()
  var constraintMap: Map[Description, Constraint[Any, Any, Any]] = Map()

  for (m <- clazz.getDeclaredMethods().filter((m) => returnTypeIsEngine(m))) {
    val engineDescription = Description.createSuiteDescription(m.getName())
    getDescription.addChild(engineDescription)
    val engine: Engine[Any] = m.invoke(instance).asInstanceOf[Engine[Any]];
    println(m.getName())
    println(engine)
    engineMap = engineMap + (engineDescription -> engine)
    for (c <- engine.constraints) {
      val name = c.params + " => " + c.expected + " " + c.because.becauseString
      val cleanedName = name.replace("(", "<").replace(")", ">");
//      println("   " + name)
      val constraintDescription = Description.createSuiteDescription(cleanedName)
      engineDescription.addChild(constraintDescription)
      constraintMap = constraintMap + (constraintDescription -> c.asInstanceOf[Constraint[Any, Any, Any]])
    }
  }

  def run(notifier: RunNotifier) {
    notifier.fireTestStarted(getDescription)
    for (ed <- getDescription.getChildren) {
      notifier.fireTestStarted(ed)
      val engine = engineMap(ed)
      for (cd <- ed.getChildren) {
        notifier.fireTestStarted(cd)
        val constraint = constraintMap(cd)
        val b = engine.makeClosureForBecause(constraint.params);
        val actual = engine.applyParam(constraint.params)
        try {
          Assert.assertEquals(constraint.expected, actual)
          notifier.fireTestFinished(cd)
        } catch {
          case e: Throwable => notifier.fireTestFailure(new Failure(cd, e))
        }
      }
      notifier.fireTestFinished(ed)
    }
    notifier.fireTestFinished(getDescription)
  }
  def returnTypeIsEngine(m: Method): Boolean = {
    val rt = m.getReturnType()
    val c = classOf[MutableEngine[Any]]
    if (c.isAssignableFrom(rt))
      return true;
    for (t <- rt.getInterfaces())
      if (c.isAssignableFrom(t))
        return true;
    return false;
  }

  def instantiate(clazz: Class[_]): Any = {
    val rm = ru.runtimeMirror(clazz.getClassLoader())
    val declaredFields = clazz.getDeclaredFields().toList
    val obj = declaredFields.find(field => field.getName() == "MODULE$") match {
      case Some(modField) => modField.get(clazz)
      case None => clazz.newInstance()
    }
    obj
  }

  trait SomeTrait { def someMethod: String }
  object SomeObject extends SomeTrait { def someMethod = "something" }

  class SomeClass extends SomeTrait { def someMethod = "something" }

  object Main {
    def main(args: Array[String]) = {
      val someClassTrait: SomeTrait = Class.forName("SomeClass").newInstance().asInstanceOf[SomeTrait]
      println("calling someClassTrait: " + someClassTrait.someMethod)
      val objectName = "SomeObject$"
      val cons = Class.forName(objectName).getDeclaredConstructors();
      cons(0).setAccessible(true);
      val someObjectTrait: SomeTrait = cons(0).newInstance().asInstanceOf[SomeTrait]
      println("calling someObjectTrait: " + someObjectTrait.someMethod)
    }
  }
}