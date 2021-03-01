package com.backwards.typelevel

import org.scalatest.matchers.must.Matchers
import org.scalatest.wordspec.AnyWordSpec

/**
 * [[https://www.tobyhobson.com/posts/shapeless/coproducts/ Shapeless Coproducts]]
 */
class CoproductErrorHandlingSpec extends AnyWordSpec with Matchers {
  "The usual way" should {
    "highlight shortcomings" in {
      sealed trait Error

      final case class BadName(name: String) extends Error

      final case class BadAge(age: Int) extends Error

      val registrationError: Error = BadAge(42)

      registrationError match {
        case BadName(name) => s"$name is bad"
        case BadAge(age) => "$age is bad"
      }

      // Shortcomings:
      // Need -Xfatal-warnings Scalac option
      // Before latest 2.13 a guard in case can obfuscate a match error
      // We must return same types in cases to avoid widening e.g. String => Any

      // Let’s say that for some reason we want to keep the same underlying types when handling the error, i.e. BadName => String and BadAge => Int.
      // We can’t easily do this using a normal pattern match:
      val matched: Any = registrationError match {
        case BadName(name) => "BAD NAME"
        case BadAge(age) => -1
      }
    }
  }

  "The Shapeless way" should {
    "use coproduct" in {
      import shapeless.{:+:, CNil, Coproduct, Poly1}

      final case class BadName(name: String)

      final case class BadAge(age: Int)

      // We need no type hierarchy to link BadName and BadAge. These error types could even come from different libraries.
      type Error = BadName :+: BadAge :+: CNil

      // E.g. of instantiating an Error
      val nameErrorExample: Error = Coproduct[Error](BadName("Bob"))

      // Pattern matching on the Coproduct?
      // There are a few ways of doing this. Our error is an Hlist which is like a tuple on steroids.
      // Therefore the simplest is to `map` or `fold` the error using something called a polymorphic function.

      // Poly1 simply means a function that accepts one parameter.
      // The code won’t compile if we forget to handle one of the scenarios - We get the type safety of sealed trait with more flexibility.
      object errorHandler extends Poly1 {
        implicit def name: Case.Aux[BadName, String] =
          at[BadName] { e => s"bad first name: ${e.name}" }

        implicit def age: Case.Aux[BadAge, String] =
          at[BadAge] { e => s"bad age: ${e.age}" }
      }

      // We put it all together by folding the error using the handler:
      val nameError: Error = Coproduct[Error](BadName("Bob"))

      val errorMessage: String = nameError fold errorHandler
      // We fold the list (HList) to get a String: BadName :+: BadAge :+: CNil => String

      println(errorMessage)

      // Again let's say that for some reason we want to keep the same underlying types when handling the error, i.e. BadName => String and BadAge => Int.
      // We can keep the different typs using Shapeless coproducts. To do so we map over the error instead of folding:
      object errorHandler2 extends Poly1 {
        implicit def name: Case.Aux[BadName, String] =
          at[BadName] { e => "BAD NAME" }

        implicit def age: Case.Aux[BadAge, Int] =
          at[BadAge] { e => -1 }
      }

      val nameError2: Error = Coproduct[Error](BadName("Bob"))

      val errorMessage2: String :+: Int :+: CNil =
        nameError2 map errorHandler2

      // However as we mapped instead of folding we get an Hlist back. Our transformation looks like this BadName :+: BadAge :+: CNil => String :+: Int :+: CNil

      // To get our String or Int we can either fold as before and transform to a single type, or we can select the type we want:

      val maybeStringError: Option[String] =
        errorMessage2.select[String]

      val maybeIntError: Option[Int] =
        errorMessage2.select[Int]
    }
  }
}