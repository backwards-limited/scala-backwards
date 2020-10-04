package com.backwards.fp.monoid

import org.scalatest.matchers.must.Matchers
import org.scalatest.wordspec.AnyWordSpec

class ListSpec extends AnyWordSpec with Matchers {
  import com.backwards.fp.monoid.ListOps._
  import com.backwards.fp.monoid.MonoidOps._

  "List Monoid" should {
    "append lists" in {
      List(1, 2, 3) |+| List(4, 5, 6) mustBe List(1, 2, 3, 4, 5, 6)
    }

    "obey identity" in {
      List(1, 2, 3, 4, 5) |+| Monoid.mzero[List[Int]] mustBe List(1, 2, 3, 4, 5)
    }

    "obey associtiativity" in {
      val a = List(1, 2, 3)
      val b = List(4, 5, 6)
      val c = List(7, 8, 9)

      a |+| b |+| c mustBe a |+| (b |+| c)
    }
  }
}