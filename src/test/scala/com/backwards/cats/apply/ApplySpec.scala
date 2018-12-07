package com.backwards.cats.apply

import cats.Apply
import org.scalatest.{MustMatchers, WordSpec}

/**
  * Apply inherits from Functor and Semigroupal and adds a method "ap"
  */
class ApplySpec extends WordSpec with MustMatchers {
  "Apply" should {
    "option" in {
      import cats.syntax.option._
      import cats.instances.option._

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