package com.backwards.fp.functor

import org.scalatest.matchers.must.Matchers
import org.scalatest.wordspec.AnyWordSpec

class ListSpec extends AnyWordSpec with Matchers {
  import com.backwards.fp.functor.Backwards.FunctorOps._
  import com.backwards.fp.functor.ListOps._

  "List Functor" should {
    "work on simple functions" in {
      List(1) fmap { x: Int => x + 1 } mustBe List(2)

      List(1, 2, 3) fmap { x: Int => x + 1 } mustBe List(2, 3, 4)
    }

    "obey identity" in {
      List(1) fmap identity mustBe List(1)
    }

    "obey composition" in {
      val f = { x: Int => x + 1 }
      val g = { x: Int => x.toString + "a" }

      { List(1) fmap f fmap g } mustBe { List(1) fmap { f andThen g } }
    }
  }
}