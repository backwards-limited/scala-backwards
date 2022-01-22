package tech.backwards.parsers

import org.scalatest.matchers.must.Matchers
import org.scalatest.wordspec.AnyWordSpec
import scala.util.parsing.combinator.{JavaTokenParsers, RegexParsers}

class ParserCombinatorSpec extends AnyWordSpec with Matchers {
  "" should {
    "" in {
      object CommentParser extends JavaTokenParsers {
        import scala.language.implicitConversions

        def singleLine: Parser[String] = "//.*".r ^^ ( _ => "")

        def multiLine: Parser[String] = """/\*.*\*/""".r ^^^ ""

        def comments: Parser[Seq[String]] = (singleLine | multiLine).*

        def commentedText : Parser[String] = comments ~> "[^\\/*]*".r <~ comments

        def empty: Parser[Seq[String]] = "" ^^^ Seq()

        def expression: Parser[String] = commentedText ~ (empty | commentedText.*) ^^ {
          case (a: String) ~ (b: Seq[String]) => a + b.mkString("")
        }

        def stripComments(str: String): Either[String, String] = {
          parseAll(expression, str) match {
            case Success(result, _) => Right(result)
            case failedOrIncomplete => Left(failedOrIncomplete.toString)
          }
        }
      }

      val r1: Either[String, String] = CommentParser.stripComments("/* a comment */")
      println(r1)

      val r2 = CommentParser.stripComments("Text Before./* level1 /* level 2 */  */")
      println(r2)
    }
  }
}