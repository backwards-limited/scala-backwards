package com.backwards.catz.semigroup

import cats.kernel.Semigroup
import org.scalatest.matchers.must.Matchers
import org.scalatest.wordspec.AnyWordSpec

/**
  * A Semigroup is a type class with a "combine" method
  * We often need to aggregate data. Hadoop/Spark map-reduce is a classic example of this (the reduce part).
  * Semigroups allow us to parallelise operations on large and small data sets, then combine the results together.
  * In essence a Semigroup encapsulates the reduce part of map reduce.
  * A Semigroup has an associative binary operation.
  * Associativity allows us to partition the data any way we want and potentially parallelise the operations.
  */
class SemigroupSpec extends AnyWordSpec with Matchers {
  "Semigroup basics" should {
    "int" in {
      import cats.instances.int._

      Semigroup[Int].combine(4, 2) mustBe 6
    }

    "string" in {
      import cats.instances.string._

      Semigroup[String].combine("Hello ", "world!") mustBe "Hello world!"
    }

    "custom" in {
      case class Error(message: String)

      object Error {
        implicit val composeError: Semigroup[Error] =
          (x: Error, y: Error) => Error(s"${x.message}, ${y.message}")
      }

      val firstError = Error("First error")
      val secondError = Error("Second error")

      Semigroup[Error].combine(firstError, secondError) mustBe Error(s"${firstError.message}, ${secondError.message}")
    }

    "custom with the help of cats" in {
      implicit val multiplicationSemigroup: Semigroup[Int] = Semigroup.instance[Int](_ * _)
      // Though slightly irrelavent regarding Scala 2.12 (see above)

      Semigroup[Int].combine(3, 4) mustBe 12
    }
  }

  "Semigroup" should {
    "" in {

    }
  }
}