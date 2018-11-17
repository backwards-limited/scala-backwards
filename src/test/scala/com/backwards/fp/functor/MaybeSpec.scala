package com.backwards.fp.functor

import scala.language.postfixOps
import org.scalatest.{MustMatchers, WordSpec}

class MaybeSpec extends WordSpec with MustMatchers {
  import FunctorOps._
  import Maybe._

  "Maybe functor" should {
    "works on Just" in {
      Just(1) fmap { x: Int => (x + 2) toString } mustBe Just("3")
    }

    "works on Nothing" in {
      Nothing[Int]() fmap { x: Int => x + 2 } mustBe Nothing[Int]()
    }

    "obey identity" in {
      Just(1) fmap identity mustBe Just(1)
      Nothing[Int]() fmap identity mustBe Nothing[Int]()
    }

    "obey composition" in {
      val a = { x: Int => x + 2 }
      val b = { x: Int => x * 2 }

      { Just(1) fmap a fmap b } mustBe { Just(1) fmap { a andThen b }}
    }

    "generically on a maybe" in {
      val maybe: Maybe[Int] = Just(1)

      maybe fmap { x: Int => (x + 2) toString } mustBe Just("3")
    }
  }

  "Maybe functor using <$>" should {
    "works on Just" in {
      { x: Int => (x + 2) toString } `<$>` Just(1) mustBe Just("3")
    }

    "works on Nothing" in {
      { x: Int => x + 2 } `<$>` Nothing[Int]() mustBe Nothing[Int]()
    }
  }
}