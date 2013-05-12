package org.autoTdd.helloScala.tests

import scala.util.parsing.combinator.JavaTokenParsers
import org.autoTdd.helloScala.engine._
import scala.util.parsing.combinator._
import org.autotdd.constraints.CodeFn
import org.autotdd.constraints.Because

object IfThenParser {

  def parser1[P, R](becauses: Map[String, Because[(P) => Boolean]], inputs: Map[String, Any], constraints: Map[String, Constraint1[P, R]], thens: Map[String, CodeFn[(P) => R, Constraint1[P,R]]]) = {
    new IfThenParser[R]() with Engine1Types[P, R].actualParser(becauses, inputs, constraints, thens,
      (b, i, r) => { val p = i(0).asInstanceOf[P]; new Constraint1[P, R](p, r.rfn(p), r, b) })
  }
  def parser2[P1, P2, R](becauses: Map[String, Because[(P1, P2) => Boolean]], inputs: Map[String, Any], constraints: Map[String, Constraint2[P1, P2, R]], thens: Map[String, CodeFn[(P1, P2) => R, Constraint2[P1,P2,R]]]) =
    new IfThenParser[R]() with Engine2Types[P1, P2, R].actualParser(becauses, inputs, constraints, thens,
      (b, i, r) => { val p1 = i(0).asInstanceOf[P1]; val p2 = i(1).asInstanceOf[P2]; new Constraint2[P1, P2, R](p1, p2, r.rfn(p1, p2), r, b) })
}

trait IfThenParser[R] extends EngineTypes[R] {

  def actualParser(becauses: Map[String, Because[B]], inputs: Map[String, Any], constraints: Map[String, C], thens: Map[String, Code], constraintMaker: (Because[B], List[Any], Code) => C) = new ActualParser[R](becauses, inputs, constraints, thens, constraintMaker);

  case class ActualParser[R](val becauses: Map[String, Because[B]], val inputs: Map[String, Any], val constraints: Map[String, C], val thens: Map[String, Code], constraintMaker: (Because[B], List[Any], Code) => C) extends JavaTokenParsers with Function[String, RorN] {

    def resultOrifThenElse: Parser[RorN] = ifThenElse | result
    def ifThenElse: Parser[RorN] = "if" ~> because ~ inputList ~ constraintList ~ yes ~ "else" ~ no ^^ { case b ~ i ~ c ~ y ~ e ~ n => Right(Node(b, i, y, n)) }

    def because: Parser[Because[B]] = ident ^^ (becauses(_))
    def inputList: Parser[List[Any]] = opt("/" ~ repsep(input, ",")) ^^ { case Some(l ~ i) => i; case None => List() }
    def input: Parser[Any] = ident ^^ (inputs(_))
    def constraintList: Parser[List[C]] = repsep(constraint, ",")

    def constraint: Parser[C] = "#" ~> because ~ inputList ~ "->" ~ ident ^^ { case b ~ i ~ junk ~ r => constraintMaker(b, i, thens(r)) }

    def yes: Parser[RorN] = ("then" ~> result | ifThenElse)
    def no: Parser[RorN] = resultOrifThenElse

    def result: Parser[RorN] = ident ^^ (x => (Left(thens(x))))

    override def apply(raw: String): RorN =
      parseAll(resultOrifThenElse, raw).get;
  }

}