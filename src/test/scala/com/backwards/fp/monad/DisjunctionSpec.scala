package com.backwards.fp.monad

import org.scalatest.{MustMatchers, WordSpec}
import com.backwards.fp.Disjunction._
import com.backwards.fp.{Disjunction, RightDisjunction}

class DisjunctionSpec extends WordSpec with MustMatchers {
  import com.backwards.fp.functor.DisjunctionOps._
  import com.backwards.fp.monad.MonadOps._

  type Or[A] = Disjunction[String, A]

  "Disjunction" should {
    "work with rights" in {
      // TODO - Taking this import up to the other causes a "flatMap" not found error!!!
      import com.backwards.fp.monad.DisjunctionOps._

      val disjunction = for {
        x <- "a".pure[Or]
        y <- "b".pure[Or]
        z <- "c".pure[Or]
      } yield x + y + z

      disjunction mustBe RightDisjunction("abc")
    }

    "work with lefts" in {
      // TODO - Taking this import up to the other causes a "flatMap" not found error!!!
      import com.backwards.fp.monad.DisjunctionOps._

      val disjunction = for {
        x <- 10.pure[Or]
        y <- left[String, Int]("boom")
        z <- 20.pure[Or]
      } yield x + y + z

      disjunction mustBe left[String, Int]("boom")
    }
  }
}