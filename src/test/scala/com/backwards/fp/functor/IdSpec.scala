package com.backwards.fp.functor

import org.scalatest.matchers.must.Matchers
import org.scalatest.wordspec.AnyWordSpec
import com.backwards.fp.Id

class IdSpec extends AnyWordSpec with Matchers {
  import com.backwards.fp.functor.Backwards.FunctorOps._
  import com.backwards.fp.functor.IdOps._

  "Id" should {
    "work on simple functions" in {
      Id(1) fmap { x: Int => x + 1 } mustBe Id(2)

      Id("one, ") fmap { s => s"${s}two" } mustBe Id("one, two")
    }

    "obey identity" in {
      Id(1) fmap identity mustBe Id(1)
    }

    "obey composition" in {
      val f = { x: Int => x + 1 }
      val g = { x: Int => x * 2 }

      { Id(2) fmap f fmap g } mustBe { Id(2) fmap (f andThen g) }
    }
  }
}