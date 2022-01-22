package tech.backwards.parsers

import scala.util.Try
import scala.util.parsing.combinator.{Parsers, RegexParsers}
import org.specs2.matcher.ParserMatchers
import org.specs2.mutable.Specification

/**
 * [[https://kerflyn.wordpress.com/2012/08/25/playing-with-scala-parser-combinator/ Simple Parser]]
 *
 *  - a ~ b parse a sequence made of a then b
 *  - a | b introduce an alternative parser that parses a or b
 *  - a? introduce an optional parser
 *  - a* introduce on optional and repeatable parser
 *  - a+ introduce a repeatable parser
 *  - a ~> b like ~ but ignore the left member (a)
 *  - a <~ b like ~ but ignore the right member (b)
 */
object ParserWithSpecs2Spec extends Specification with ParserMatchers {
  val parsers: Parsers = NumberParser

  "Number parser" should {
    import NumberParser._

    "parse successfully" in {
      val Success(result, _) = parseAll(number, "42")

      result must_=== 42
    }

    "fail to parse" in {
      val Failure(msg, _) = parseAll(number, "whoops")

      msg must_=== "Constructor function not defined at "
    }
  }

  "Expression parser" should {
    "parse successfully" in {
      ExprParser("2 + 3") must beSome(Add(Const(2), Const(3)))
    }
  }
}

object NumberParser extends RegexParsers {
  object ParsedInt {
    def unapply(str: String): Option[Int] = Try(str.toInt).toOption
  }

  val number: Parser[Int] =
    """-?\d*""".r ^? {
      case ParsedInt(x) => x
    }
}

sealed abstract class Expr

final case class Const(value: Int) extends Expr

case object X extends Expr

final case class Add(x: Expr, y: Expr) extends Expr

final case class Mul(x: Expr, y: Expr) extends Expr

object ExprParser extends RegexParsers {
  lazy val expression: Parser[Expr] = operation1

  lazy val operation1: Parser[Expr] = operation2 ~ rep("+" ~ operation2) ^^ {
    case op ~ list => list.foldLeft(op) {
      case (x, "+" ~ y) => Add(x, y)
    }
    case _ => sys.error("Whoops")
  }

  lazy val operation2: Parser[Expr] = operand ~ rep("*" ~ operand) ^^ {
    case op ~ list => list.foldLeft(op) {
      case (x, "*" ~ y) => Mul(x, y)
    }
    case _ => sys.error("Whoops")
  }

  lazy val operand: Parser[Expr] = constant | variable

  lazy val variable: Parser[Expr] = "x" ^^ { _ => X }

  lazy val constant: Parser[Expr] = """-?\d+""".r ^^ { s => Const(s.toInt) }

  def apply(input: String): Option[Expr] = parseAll(expression, input) match {
    case Success(result, _) => Some(result)
    case NoSuccess(_, _)    => None
    case _ => sys.error("Whoops")
  }
}