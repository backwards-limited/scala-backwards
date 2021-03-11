package com.backwards.parsers

import java.io.{FileReader, Reader, StringReader}
import scala.util.Using
import scala.util.matching.Regex
import org.scalatest.matchers.must.Matchers
import org.scalatest.wordspec.AnyWordSpec
import scala.util.parsing.combinator._
import cats.implicits._

/**
 * [[https://poundblog.wordpress.com/2013/06/06/a-scala-parser-combinator-grammar-for-csv/ A Scala Parser Combinator Grammar for CSV]]
 *
 *  - p1 ~ p2  // sequencing: must match p1 followed by p2
 *  - p1 | p2  // alternation: must match either p1 or p2, with preference given to p1
 *  - p1.?     // optionality: may match p1 or not
 *  - p1.+     // repetition: matches 1 or more repetitions of p1
 *  - p1.*     // repetition: matches any number of repetitions of p1
 *  - p1 ~> p2 // a parser combinator for sequential composition which keeps only the right result.
 *  - p1 <~ p2 // a parser combinator for sequential composition which keeps only the left result.
 */
class CSVParserSpec extends AnyWordSpec with Matchers {
  "" should {
    "" in {
      val csv =
        """Year,Month,Day
          |2013,June,07
          |""".stripMargin

      println(CSVParser(new StringReader(csv)))
    }
  }

  object CSVParser extends RegexParsers {
    override val skipWhitespace: Boolean = false

    override val whiteSpace: Regex = """[ \t]""".r

    lazy val file: Parser[List[List[String]]] = hdr ~ row.+ ^^ {
      case header ~ rows => header :: rows
    }

    lazy val hdr: Parser[List[String]] = row

    lazy val row: Parser[List[String]] = field ~ ("," ~> field).* <~ "\r".? <~ "\n" ^^ {
      case f ~ fields => f :: fields
    }

    lazy val eof: Parser[String] = "\\z".r

    lazy val field: Parser[String] = text | string | empty

    lazy val text: Parser[String] = "[^,\n\r\"]".r.+ ^^ {
      text => text mkString ""
    }

    // Remove embracing quotation marks
    lazy val string: Parser[String] = "\"" ~> ("\"\"" | "[^\"]".r).* <~ "\"" ^^ {
      string => string mkString "" replaceAll ("\"\"", "\"")
    }

    lazy val empty: Parser[String] = ""

    def apply(reader: Reader): String Either Any =
      parseAll(file, reader) match {
        case Success(result, next) => result.asRight
        case NoSuccess(msg, next) => msg.asLeft
      }
  }
}