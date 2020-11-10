package com.backwards.fp.monaderror

import scala.util.Try
import cats.MonadError
import org.scalatest.matchers.must.Matchers
import org.scalatest.wordspec.AnyWordSpec

class MonadErrorSpec extends AnyWordSpec with Matchers {
  // Bunch of dummy values to avoid going into specifics that are not relevant:

  final case class Json()

  final case class ParseException(str: String) extends Throwable

  val success: Boolean = true
  val result: Json = Json()
  val content: String = ""

  "MonadError" should {
    "abstract over error handling monads" in {
      def toJsonOpt(str: String): Option[Json] = {
        // Some code doing the actual parsing
        if (success) Some(result) else None
      }

      def toJsonEither(str: String): String Either Json =
        if (success) Right(result)
        else Left("Could not parse JSON String")

      /*
      So far so good. But we cannot easily combine error types e.g this will not compile:

      def readFile(): Try[String] = ???

      def toJson(str: String): Throwable Either Json = ???

      for {
        fileContent <- readFile()
        parsedFile <- toJson(fileContent)
      } yield parsedFile

      We would like to have:
      def toJson[F[_]](str: String): F[Json] =
        if (success) SUCCESS_CASE else ERROR_CASE

      This allows users to specify the desired monad when calling the function.

      MonadError is a type class that abstracts over error handling monads, making it possible to raise or handle errors functionally while keeping the monad generic.
      */

      def toJson[F[_]](str: String)(implicit M: MonadError[F, Throwable]): F[Json] =
        if (success) M.pure(result)
        else M.raiseError(ParseException("Could not parse JSON String"))

      import cats.implicits._

      val parsedTry: Try[Json] =
        toJson[Try](content)

      val parsedEither: Throwable Either Json =
        toJson[Throwable Either *](content)
    }

    "allow error to be abstracted" in {
      trait UIError[A] {
        def errorFromString(str: String): A

        def errorFromThrowable(thr: Throwable): A
      }

      implicit val throwableInstance: UIError[Throwable] = new UIError[Throwable] {
        override def errorFromString(str: String): Throwable =
          new Throwable(str)

        override def errorFromThrowable(thr: Throwable): Throwable =
          thr
      }

      implicit val stringInstance: UIError[String] = new UIError[String] {
        override def errorFromString(str: String): String =
          str

        override def errorFromThrowable(thr: Throwable): String =
          s"""${thr.getMessage}: ${thr.getCause}
             |${thr.getStackTrace}""".stripMargin
      }

      def toJson[F[_], E](str: String)(implicit M: MonadError[F, E], E: UIError[E]): F[Json] =
        if (str == content) M.pure(result)
        else M.raiseError(E.errorFromString("Could not parse JSON String"))

      import cats.implicits._

      val parsedTry: Try[Json] =
        toJson[Try, Throwable](content)

      val parsedEither: Either[String, Json] =
        toJson[String Either *, String](content)

      println(parsedTry)
      println(parsedEither)

      println(toJson[Try, Throwable]("{"))
      println(toJson[String Either *, String]("{"))
    }
  }
}