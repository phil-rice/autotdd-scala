package org.autoTdd.helloScala.tests
import scala.util.parsing.combinator.JavaTokenParsers
import org.autoTdd.helloScala.engine._

class IfThenParser[B, R](val becauses: Map[String, B], val inputs: Map[String, R], val thens: Map[String, R]) extends JavaTokenParsers with Function[String, Node[B, R]] {
  type TE = Either[RFnAndDesc[R], Node[B, R]]

  def ifThenElse: Parser[TE] = "if" ~> because ~ inputList ~ yes ~ "else" ~ no ^^ { case b ~ i ~ y ~ e ~ n => Right(Node(b, b.toString, i, y, n)) }

  def because: Parser[B] = ident ^^ (becauses(_))
  def inputList: Parser[List[Any]] = opt("/" ~ repsep(input, ",")) ^^ { case Some(l ~ i) => println("inputs: " + l + i); i; case None => List() }
  def input: Parser[Any] = ident ^^ (inputs(_))

  def yes: Parser[TE] = ("then" ~> result | ifThenElse)
  def no: Parser[TE] = result | ifThenElse
  def result: Parser[TE] = ident ^^ (x => (Left(RFnAndDesc[R](thens(x), thens(x).toString))))

  override def apply(raw: String): Node[B, R] =
    parseAll(ifThenElse, raw).get.right.get;
}

