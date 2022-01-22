package tech.backwards.fp.typeclass

import org.scalatest.matchers.must.Matchers
import org.scalatest.wordspec.AnyWordSpec
import tech.backwards.fp.typeclass.Order._
import tech.backwards.fp.typeclass.OrderInstances._

class OrderSpec extends AnyWordSpec with Matchers {
  "Total order" should {
    "compare ints" in {
      less(5, 10) mustEqual true
      less(10, 5) mustEqual false
    }

    "compare strings" in {
      less("5", "9") mustEqual true
      less("9", "5") mustEqual false
    }

    "compare list of ints" in {
      less(List(5, 10), List(10, 20)) mustEqual true
    }
  }
}