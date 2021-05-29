package com.backwards.typelevel

import java.net.InetAddress
import java.time.Instant
import scala.util.Try
import cats.implicits.catsSyntaxEitherId
import org.scalatest.matchers.must.Matchers
import org.scalatest.wordspec.AnyWordSpec
import shapeless.{::, Generic, HList, HNil}

/**
 * [[https://dev.to/devkat/csv-parsing-with-scala-and-shapeless-5hho CSV parsing with Scala and shapeless]]
 */
class CSVParsingSpec extends AnyWordSpec with Matchers {
  "" should {
    "" in {
      /*
      Web server log format:
      - Remote IP address
      - Time in milliseconds (Unix time stamp)
      - Request path
      - User agent

      e.g.
      "1.2.3.4","1466585706027","/foo","Chrome"
      */

      final case class LogEntry(ip: String, time: Instant, req: String, userAgent: String)

      type Error = String

      trait Parser[A] {
        def apply(s: String): Error Either A
      }

      // A convenient parser
      trait TryParser[A] extends Parser[A] {
        def parse(s: String): A

        def apply(s: String): Error Either A =
          Try(parse(s)).fold(_.getMessage.asLeft, Right.apply)
      }

      object LogParsers {
        implicit val instantParser: TryParser[Instant] =
          Instant parse

        implicit val inetAddressParser: TryParser[InetAddress] =
          InetAddress getByName
      }

      trait LineParser[Out] {
        def apply(xs: List[String]): List[Error] Either Out
      }

      object LineParser {
        implicit val hnilParser: LineParser[HNil] = {
          case Nil => Right(HNil)
          case h +: t => Left(List(s"""Expected end of line, got "$h"."""))
          case _ => sys.error("Whoops")
        }

        implicit def hconsParser[H: Parser, T <: HList: LineParser]: LineParser[H :: T] = {
          case Nil =>
            Left(List("Expected list element."))

          case h +: t =>
            val head = implicitly[Parser[H]].apply(h)
            val tail = implicitly[LineParser[T]].apply(t)

            (head, tail) match {
              case (Left(error), Left(errors)) => Left(error :: errors)
              case (Left(error), Right(_)) => Left(error :: Nil)
              case (Right(_), Left(errors)) => Left(errors)
              case (Right(h), Right(t)) => Right(h :: t)
            }

          case _ =>
            sys.error("Whoops")
        }

        implicit def adtParser[Out, R <: HList](
          implicit gen: Generic[Out] { type Repr = R },
          reprParser: LineParser[R]
        ): LineParser[Out] =
          (xs: List[String]) => reprParser.apply(xs).map(gen.from)

        def apply[A: LineParser]: LineParser[A] =
          implicitly[LineParser[A]]

        def apply[A: LineParser](xs: List[String]): List[String] Either A =
          LineParser[A].apply(xs)
      }

      /*class CsvReader[T : LineParser] {
        private lazy val csvParser = new CSVParser(new DefaultCSVFormat() {})

        private def parseLine(line: String): Either[List[String], T] =
          csvParser.parseLine(line) match {
            case Some(fields) => LineParser[T](fields)
            case None => Left(List(s"Invalid line: $line"))
          }

        /**
         * Transforms a flow of strings into a flow of CSV records.
         * Invalid records will be skipped.
         *
         * @param source The source.
         * @tparam Mat The materialized value type.
         * @return The transformed source.
         */
        def read[Mat](source: Source[String, Mat]): Source[T, Mat] =
          source
            .map(parseLine)
            .map(_.fold(errors => { println(errors); None }, Some(_)))
            .collect { case Some(t) => t }
      }*/
    }
  }
}