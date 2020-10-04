package com.backwards.fp.monad

import org.scalatest.matchers.must.Matchers
import org.scalatest.wordspec.AnyWordSpec
import com.backwards.fp.{Just, Maybe, Nothing}

class MaybeSpec extends AnyWordSpec with Matchers {
  import com.backwards.fp.functor.FunctorOps._
  import com.backwards.fp.functor.MaybeOps._
  import com.backwards.fp.monad.MaybeOps._
  import com.backwards.fp.monad.MonadOps._

  "Maybe Monad" should {
    "work with Just" in {
      val maybe: Maybe[Int] = for {
        x <- 10.pure
        y <- Just(20)
      } yield x + y

      maybe mustBe Just(30)
    }

    "work with Nothing" in {
      val maybe: Maybe[Int] = for {
        x <- 10.pure
        y <- Nothing[Int]()
      } yield x + y

      maybe mustBe Nothing()
    }
  }
}