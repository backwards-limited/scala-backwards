package tech.backwards.catz.monoid

import cats.kernel.Monoid
import org.scalatest.matchers.must.Matchers
import org.scalatest.wordspec.AnyWordSpec

/**
  * A Monoid is a Semigroup with a neutral element, that we'll call empty
  */
class MonoidSpec extends AnyWordSpec with Matchers {
  "Monoid basics" should {
    "int" in {
      import cats.instances.int._

      Monoid[Int].empty mustBe 0

      Monoid[Int].combineAll(List(3, 4, 5)) mustBe 12
      Monoid[Int].combineAll(List.empty[Int]) mustBe 0
    }

    "string" in {
      import cats.instances.string._

      Monoid[String].empty mustBe ""

      Monoid[String].combineAll(List("H", "e", "l", "l", "o", "!")) mustBe "Hello!"
      Monoid[String].combineAll(List.empty[String]) mustBe ""
    }
  }

  "Monoid" should {
    "allow for basic map/reduce" in {
      def mapReduce[A, B: Monoid](seq: Seq[A])(op: A => B): B =
        seq.map(op).foldLeft(Monoid[B].empty)(Monoid[B].combine)

      import cats.instances.int._ // Monoid implementation for ints

      val result = mapReduce(Seq(1, 2, 3))(_ * 2)
      result mustBe 12
    }
  }
}