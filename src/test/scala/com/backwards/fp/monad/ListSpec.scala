package com.backwards.fp.monad

import org.scalatest.matchers.must.Matchers
import org.scalatest.wordspec.AnyWordSpec

class ListSpec extends AnyWordSpec with Matchers {
  import com.backwards.fp.functor.FunctorOps._
  import com.backwards.fp.functor.ListOps._
  import com.backwards.fp.monad.ListOps._
  import com.backwards.fp.monad.MonadOps._

  "List Monad" should {
    "work" in {
      val list = List(1, 2, 3) >>= { x =>
        List(4, 5) fmap { y =>
          (x, y)
        }
      }

      list mustBe List((1, 4), (1, 5), (2, 4), (2, 5), (3, 4), (3, 5))
    }

    "flatten" in {
      val list = List(List(1, 2, 3), List(4, 5, 6), List(7, 8, 9)) >>= identity

      list mustBe (1 to 9).toList
    }
  }
}