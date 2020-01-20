package com.backwards.cats.applicative

import scala.concurrent.Await
import scala.language.postfixOps
import cats.Applicative
import org.scalatest.concurrent.ScalaFutures
import org.scalatest.matchers.must.Matchers
import org.scalatest.wordspec.AnyWordSpec

/**
  * Apply with the addition of the method "pure" forms an Applicative.
  * "pure" just takes an element and lifts it into the Applicative Functor.
  * In essence an Applicative lets us peform N operations independently, then it aggregates the results for us.
  */
class ApplicativeSpec extends AnyWordSpec with Matchers with ScalaFutures {
  "Applicative basics" should {
    "option" in {
      import cats.instances.option._

      Applicative[Option].pure(1) mustBe Option(1)
    }

    "list" in {
      import cats.instances.list._

      Applicative[List].pure(1) mustBe List(1)
    }
  }

  "Applicative" should {
    "example 1" in {
      import scala.concurrent.ExecutionContext.Implicits.global
      import scala.concurrent.Future
      import scala.concurrent.duration._
      import cats.instances.future._

      def fetchFirstName: Future[String] = {
        println("Fetching first name")
        Future.successful("Bruce")
      }

      def fetchLastName: Future[String] = {
        println("Fetching last name")
        Future.successful("Wayne")
      }

      val eventualFullName = Applicative[Future].map2(fetchFirstName, fetchLastName)(_ + " " + _)

      whenReady(eventualFullName) { fullName =>
        fullName mustBe "Bruce Wayne"
      }

      // And failing to fetch first name:
      def failFetchFirstName: Future[String] = {
        println("Fetching first name")
        Future.failed(new Exception("Whoops"))
      }

      val failEventualFullName = Applicative[Future].map2(failFetchFirstName, fetchLastName)(_ + " " + _)

      the [Exception] thrownBy {
        Await.result(failEventualFullName, 3 seconds)
      } must have message "Whoops"
    }

    "example 2" in {
      import cats.data.Validated
      import cats.data.Validated.{Invalid, Valid}
      import cats.instances.list._
      import cats.syntax.validated._

      type Errors = List[String]

      def validate(key: String, value: String): Errors Validated String = {
        if (value.nonEmpty) value.valid else List(s"$key is empty").invalid
      }

      val errorOrName = Applicative[Errors Validated *].map2(validate("first name", "john"), validate("last name", "doe"))(_ + " " + _)
      errorOrName match {
        case Invalid(errors) => println("errors: " + errors.mkString(", "))
        case Valid(fullName) => println(fullName)
      }
    }
  }
}