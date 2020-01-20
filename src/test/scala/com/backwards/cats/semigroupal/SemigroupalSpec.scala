package com.backwards.cats.semigroupal

import cats.Semigroupal
import org.scalatest.matchers.must.Matchers
import org.scalatest.wordspec.AnyWordSpec

/**
  * Cartesian
  */
class SemigroupalSpec extends AnyWordSpec with Matchers {
  "Semigroupal" should {
    "option" in {
      import cats.instances.option._
      import cats.syntax.option._

      Semigroupal[Option].product(1.some, 2.some) mustBe Option((1, 2))
      Semigroupal[Option].product(1.some, none) mustBe None
    }

    "list" in {
      import cats.instances.list._

      Semigroupal[List].product(List(1, 2), List(3, 4)) mustBe List((1, 3), (1, 4), (2, 3), (2, 4))
      Semigroupal[List].product(List.empty, List(3, 4)) mustBe List()
    }
  }
}