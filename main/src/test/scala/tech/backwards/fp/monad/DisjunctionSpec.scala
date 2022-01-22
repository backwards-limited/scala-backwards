package tech.backwards.fp.monad

import tech.backwards.fp.{Disjunction, RightDisjunction}
import org.scalatest.matchers.must.Matchers
import org.scalatest.wordspec.AnyWordSpec
import tech.backwards.fp.Disjunction._
import tech.backwards.fp.RightDisjunction

class DisjunctionSpec extends AnyWordSpec with Matchers {
  import tech.backwards.fp.functor.DisjunctionOps._
  import tech.backwards.fp.monad.DisjunctionOps._

  type Or[A] = Disjunction[String, A]

  "Disjunction" should {
    "work with rights" in {
      val disjunction = for {
        x <- "a".pure[Or]
        y <- "b".pure[Or]
        z <- "c".pure[Or]
      } yield x + y + z

      disjunction mustBe RightDisjunction("abc")
    }

    "work with lefts" in {
      val disjunction = for {
        x <- 10.pure[Or]
        y <- left[String, Int]("boom")
        z <- 20.pure[Or]
      } yield x + y + z

      disjunction mustBe left[String, Int]("boom")
    }
  }
}