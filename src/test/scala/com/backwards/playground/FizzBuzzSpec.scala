package com.backwards.playground

import cats.implicits._
import cats.kernel.Monoid
import org.scalatest.MustMatchers
import org.scalatest.wordspec.AnyWordSpec

class FizzBuzzSpec extends AnyWordSpec with MustMatchers {
  "FizzBuzz" should {
    "work - prefer this one" in {
      val f3: PartialFunction[Int, String] = {
        case x if x % 3 == 0 => "Fizz"
      }

      val f5: PartialFunction[Int, String] = {
        case x if x % 5 == 0 => "Buzz"
      }

      val f15: PartialFunction[Int, String] = {
        val f15: Int => Option[String] =
          x => (f3 lift x, f5 lift x) mapN Monoid[String].combine

        Function unlift f15
      }

      val fizzBuzz: Int => String = x => {
        (f15 orElse f5 orElse f3) lift x getOrElse x.toString
      }

      f3(3) mustBe "Fizz"
      a [MatchError] should be thrownBy f3(4)

      f5(5) mustBe "Buzz"
      a [MatchError] should be thrownBy f5(4)

      f15(15) mustBe "FizzBuzz"
      a [MatchError] should be thrownBy f15(4)

      fizzBuzz(3) mustBe "Fizz"
      fizzBuzz(5) mustBe "Buzz"
      fizzBuzz(15) mustBe "FizzBuzz"
    }
  }

  "work - don't really like this one (I didn't either bother to add assertions)" ignore {
    def f(divisor: Int, result: Int => String): PartialFunction[Int, String] = {
      case i if i % divisor == 0 => result(i)
    }
    val f3  = f(3,  _ => "Fizz")
    val f5  = f(5,  _ => "Buzz")
    val f15 = f(15, x => f3(x) + f5(x))
    val id  = f(1,  _.toString)

    val fizzBuzz = f15 orElse f3 orElse f5 orElse id

    (1 to 20).map(fizzBuzz.andThen(println(_)))
    // OR
    println("-----------------------")
    LazyList.from(1).map(fizzBuzz).take(20).foreach(println)
  }
}