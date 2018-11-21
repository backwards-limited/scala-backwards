package com.backwards.fp.monad

import org.scalatest.{MustMatchers, WordSpec}
import com.backwards.fp.{Just, Maybe, Nothing}

class MaybeSpec extends WordSpec with MustMatchers {
  import com.backwards.fp.monad.MaybeOps._
  import com.backwards.fp.monad.MonadOps._
  import com.backwards.fp.functor.FunctorOps._
  import com.backwards.fp.functor.MaybeOps._

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