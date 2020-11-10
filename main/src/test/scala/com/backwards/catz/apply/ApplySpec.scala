package com.backwards.catz.apply

import cats.Apply
import org.scalatest.matchers.must.Matchers
import org.scalatest.wordspec.AnyWordSpec

/**
  * Apply inherits from Functor and Semigroupal and adds a method "ap"
  */
class ApplySpec extends AnyWordSpec with Matchers {
  "Apply" should {
    "option" in {
      import cats.instances.option._
      import cats.syntax.option._

      val ff: Option[String => Int] = Option(_.length)

      Apply[Option].ap(ff)("Hello!".some) mustBe Option(6)
    }

    "list" in {
      import cats.instances.list._

      val ff: List[Int => Boolean] = List(_ > 2)

      Apply[List].ap(ff)(List(0, 1, 2, 3, 4)) mustBe List(false, false, false, true, true)
    }

    "list again" in {
      import cats.instances.list._

      val ff: List[Int => Boolean] = List(_ > 2, _ > 10)

      Apply[List].ap(ff)(List(0, 1, 2, 3, 4)) mustBe List(false, false, false, true, true,
                                                          false, false, false, false, false)
    }
  }
}