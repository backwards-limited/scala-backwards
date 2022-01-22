package tech.backwards.fp.functor

import scala.language.postfixOps
import tech.backwards.fp.{Just, Maybe, Nothing}
import org.scalatest.matchers.must.Matchers
import org.scalatest.wordspec.AnyWordSpec
import tech.backwards.fp.Nothing

class MaybeSpec extends AnyWordSpec with Matchers {
  import tech.backwards.fp.functor.FunctorOps._
  import tech.backwards.fp.functor.MaybeOps._

  "Maybe Functor" should {
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

  "Maybe Functor using <$>" should {
    "works on Just" in {
      { x: Int => (x + 2) toString } `<$>` Just(1) mustBe Just("3")
    }

    "works on Nothing" in {
      { x: Int => x + 2 } `<$>` Nothing[Int]() mustBe Nothing[Int]()
    }
  }
}