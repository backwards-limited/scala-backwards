package com.backwards.cats.applicative

import cats.Applicative
import org.scalatest.{MustMatchers, WordSpec}

/**
  * Apply with the addition of the method "pure" forms an Applicative.
  * "pure" just takes an element and lifts it into the Applicative Functor.
  */
class ApplicativeSpec extends WordSpec with MustMatchers {
  "Applicative" should {
    "option" in {
      import cats.instances.option._

      Applicative[Option].pure(1) mustBe Option(1)
    }

    "list" in {
      import cats.instances.list._

      Applicative[List].pure(1) mustBe List(1)
    }
  }
}