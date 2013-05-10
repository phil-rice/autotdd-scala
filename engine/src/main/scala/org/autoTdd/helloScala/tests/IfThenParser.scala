package org.autoTdd.helloScala.tests

import scala.util.parsing.combinator.JavaTokenParsers
import org.autoTdd.helloScala.engine._
import scala.util.parsing.combinator._
import org.autotdd.constraints.CodeFn
import org.autotdd.constraints.Because

object IfThenParser {

  def parser1[P, R](becauses: Map[String, Because[(P) => Boolean]], inputs: Map[String, Any], thens: Map[String, CodeFn[(P) => R]]) =
    new IfThenParser[R]() with Engine1Types[P, R].actualParser(becauses, inputs, thens)
  def parser2[P1, P2, R](becauses: Map[String, Because[(P1, P2) => Boolean]], inputs: Map[String, Any], thens: Map[String, CodeFn[(P1, P2) => R]]) =
    new IfThenParser[R]() with Engine2Types[P1, P2, R].actualParser(becauses, inputs, thens)

}

trait IfThenParser[R] extends EngineTypes[R] {

  def actualParser(becauses: Map[String, Because[B]], inputs: Map[String, Any], thens: Map[String, CodeFn[RFn]]) = new ActualParser[R](becauses, inputs, thens);

  case class ActualParser[R](val becauses: Map[String, Because[B]], val inputs: Map[String, Any], val thens: Map[String, CodeFn[RFn]]) extends JavaTokenParsers with Function[String, RorN] {

    def resultOrifThenElse: Parser[RorN] = ifThenElse | result
    def ifThenElse: Parser[RorN] = "if" ~> because ~ inputList ~ yes ~ "else" ~ no ^^ { case b ~ i ~ y ~ e ~ n => Right(Node(b, i, List(), y, n)) }

    def because: Parser[Because[B]] = ident ^^ (becauses(_))
    def inputList: Parser[List[Any]] = opt("/" ~ repsep(input, ",")) ^^ { case Some(l ~ i) => println("inputs: " + l + i); i; case None => List() }
    def input: Parser[Any] = ident ^^ (inputs(_))

    def yes: Parser[RorN] = ("then" ~> result | ifThenElse)
    def no: Parser[RorN] = resultOrifThenElse
    def result: Parser[RorN] = ident ^^ (x => (Left(thens(x))))

    override def apply(raw: String): RorN =
      parseAll(resultOrifThenElse, raw).get;
  }

}