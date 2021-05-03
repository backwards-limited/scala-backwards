package com.backwards.parsers

import java.io.{FileReader, Reader}
import scala.util.Using
import scala.util.chaining.scalaUtilChainingOps
import scala.util.parsing.combinator._
import better.files.Resource
import cats.Show
import cats.implicits._
import org.scalatest.wordspec.AnyWordSpec

/**
 * Parsing rules:
 *  1. Each parser written as a string (such as: "{" or ":" or "null") returns the parsed string itself.
 *  1. Regular expression parsers such as """[a-zA-Z_]\w*""".r also return the parsed string itself.
 *     The same holds for regular expression parsers such as stringLiteral or floatingPointNumber, which
 *     are inherited from trait JavaTokenParsers.
 *  1. A sequential composition P~Q returns the results of both P and of Q.
 *     These results are returned in an instance of a case class that is also written ~.
 *     So if P returns "true" and Qreturns "?", then
 *     the sequential composition P~Q returns ~("true", "?"), which prints as(true~?).
 *  1. An alternative composition P | Q returns the result of either P or Q, whichever one succeeds.
 *  1. A repetition rep(P) or repsep(P, separator) returns a list of the results of all runs of P.
 *  1. An option opt(P) returns an instance of Scala's Option type.
 *     It returns Some(R) if P succeeds with result R and None if P fails.
 */
class ParserCombinatorBookSpec extends AnyWordSpec {
  "Parser combinators" should {
    "describe an arithmetic DSL with JavaTokenParsers" in {
      /**
       * {{{
       *   expr ::= term \{"+" term | "-" term\}
       *
       *   term ::= factor \{"*" factor | "/" factor\}
       *
       *   factor ::= floatingPointNumber | "(" expr ")"
       * }}}
       */
      object Arithmetic extends JavaTokenParsers {
        val eval: String => ParseResult[Any] =
          parseAll(expr, _)

        lazy val expr: Parser[Any] = term ~ rep("+" ~ term | "-" ~ term)

        lazy val term: Parser[Any] = factor ~ rep("*" ~ factor | "/" ~ factor)

        lazy val factor: Parser[Any] = floatingPointNumber | "(" ~ expr ~ ")"
      }

      import Arithmetic._

      val input1 = "2 * (3 + 7)"

      println("\nInput:" + input1)
      println("Parsed:" + parseAll(expr, input1))

      val input2 = "3 + (7 - 2)"

      println("\nInput:" + input2)
      println("Parsed:" + eval(input2))

      val input3 = "3 + (7 - 2))"

      println("\nBad Input:" + input3)
      println("Parsed:" + eval(input3))
    }

    "describe an arithmetic DSL with JavaTokenParsers without any backtracking, where ~! is used instead of ~" in {
      object Arithmetic extends JavaTokenParsers {
        val eval: String => ParseResult[Any] =
          parseAll(expr, _)

        lazy val expr: Parser[Any] = term ~! rep("+" ~! term | "-" ~! term)

        lazy val term: Parser[Any] = factor ~! rep("*" ~! factor | "/" ~! factor)

        lazy val factor: Parser[Any] = floatingPointNumber | "(" ~! expr ~! ")"
      }

      import Arithmetic._

      val input = "3 + (7 - 2)"
      println("\nInput without backtracking:" + input)
      println("Parsed without backtracking:" + eval(input))
    }

    "describe an arithmetic DSL with RegexParsers" in {
      /**
       * {{{
       *   expr ::= term \{"+" term | "-" term\}
       *
       *   term ::= factor \{"*" factor | "/" factor\}
       *
       *   factor ::= floatingPointNumber | "(" expr ")"
       * }}}
       */
      object Arithmetic extends RegexParsers {
        implicit val parseResultShow: Show[ParseResult[Any]] =
          Show.show[ParseResult[Any]](r => r.toString)

        val eval: String => ParseResult[Any] =
          parseAll(expr, _)

        lazy val expr: Parser[Any] = term ~ rep("+" ~ term | "-" ~ term)

        lazy val term: Parser[Any] = factor ~ rep("*" ~ factor | "/" ~ factor)

        // Following regex is copied from JavaTokenParsers.floatingPointNumber
        lazy val factor: Parser[Any] = """-?(\d+(\.\d*)?|\d*\.\d+)([eE][+-]?\d+)?[fFdD]?""".r | "(" ~ expr ~ ")"

        def debug[R: Show](input: String, f: String => R): R =
          input.tap(in => println(s"\nInput: $in")).pipe(in => f(in)).tap(r => println(s"Parsed: ${r.show}"))
      }

      import Arithmetic._

      debug("2 * (3 + 7)", parseAll(expr, _))

      debug("3 + (7 - 2)", eval)

      debug("3 + (7 - 2))", eval)
    }

    "describe a JSON DSL" in {
      /**
       * {{{
       *   value    ::= obj | arr | stringLiteral | floatingPointNumber | "null" | "true" | "false"
       *   obj      ::= "{" [members] "}"
       *   arr      ::= "[" [values] "]"
       *   members  ::= member \{"," member\}
       *   member   ::= stringLiteral ":" value
       *   values   ::= value \{"," value\}
       * }}}
       */
      object Json extends JavaTokenParsers {
        val eval: Reader => ParseResult[Any] =
          parseAll(value, _)

        lazy val value: Parser[Any] = obj | arr | stringLiteral | floatingPointNumber | "null" | "true" | "false"

        lazy val obj: Parser[Any] = "{" ~ repsep(member, ",") ~ "}"

        lazy val arr: Parser[Any] = "[" ~ repsep(value, ",") ~ "]"

        lazy val member: Parser[Any] = stringLiteral ~ ":" ~ value
      }

      import Json._

      val scala.util.Success(result: Json.ParseResult[Any]) =
        Using(new FileReader(Resource.getUrl("address-book.json").getFile))(parseAll(value, _))

      println(result)

      println(Using(new FileReader(Resource.getUrl("address-book.json").getFile))(eval))
    }

    """It would be much better to map a JSON object into an internal Scala representation that represents the meaning of the JSON value.
      |A more natural representation would be as follows:
      | - A JSON object is represented as a Scala map of type Map[String, Any]. Every member is represented as a key/value binding in the map.
      | - A JSON array is represented as a Scala list of type List[Any].
      | - A JSON string is represented as a Scala String.
      | - A JSON numeric literal is represented as a Scala Double.
      | - The values true, false, and null are represented as the Scala values with the same names.
      |
      | The ^^ operator transforms the result of a parser.""".stripMargin in {
      /*
      As an example, here is a parser that parses a floating point number and converts it to a Scala value of type Double:
      floatingPointNumber ^^ (_.toDouble)

      And here is a slightly improved version of our Json Parser where we have enhanced "obj"
      */
      object Json extends JavaTokenParsers {
        def eval(in: Reader): ParseResult[Any] =
          parseAll(value, in)

        def eval(in: String): ParseResult[Any] =
          parseAll(value, in)

        lazy val value: Parser[Any] =
          obj | arr | stringLiteral | floatingPointNumber ^^ (_.toDouble) | "null"  ^^ (_ => null) | "true"  ^^ (_ => true) | "false" ^^ (_ => false) | failure("Illegal start of value")

        lazy val obj: Parser[Map[String, Any]] =
          "{" ~> repsep(member, ",") <~ "}" ^^ (Map(_: _*))

        lazy val arr: Parser[List[Any]] =
          "[" ~> repsep(value, ",") <~ "]"

        lazy val member: Parser[(String, Any)] =
          stringLiteral ~ ":" ~ value ^^
            { case name ~ ":" ~ value => (name, value) }
      }

      import Json._

      val scala.util.Success(result: Json.ParseResult[Any]) =
        Using(new FileReader(Resource.getUrl("address-book.json").getFile))(eval)

      println(result)

      println("\nExample of parsing bad Json:")
      println(eval("""{ "bad-json": Bob, "ok": "Bob" }"""))
    }

    "describe RegexParsers" in {
      /*
      trait RegexParsers extends Parsers

      This trait is more specialized than trait Parsers in that it only works for inputs that are sequences of characters:
      type Elem = Char

      It defines two methods, literal and regex, with the following signatures:

      implicit def literal(s: String): Parser[String] = ...

      implicit def regex(r: Regex): Parser[String] = ...


      Note that both methods have an implicit modifier, so they are automatically applied whenever a String or Regex is given but a Parser is expected.
      That's why you can write string literals and regular expressions directly in a grammar, without having to wrap them with one of these methods.
      For instance, the parser
      "(" ~ expr ~ ")"

      will be automatically expanded to
      literal("(") ~ expr ~ literal(")")
      */
    }
  }
}