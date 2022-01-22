package tech.backwards.collection

import org.scalatest.matchers.must.Matchers
import org.scalatest.wordspec.AnyWordSpec

class MapOpsSpec extends AnyWordSpec with Matchers with MapOps {
  "Map ops" should {
    "convert Map to Properties" in {
      """val p: java.util.Properties = Map[String, Int]("one" -> 1, "two" -> 2)""" must compile
    }

    "update map for a given function" in {
      val m = Map("one" -> 1, "two" -> 2)

      m.update("one", 0)(_ + 10) must contain ("one" -> 11)
      m.update("three", 0)(_ + 10) must contain ("three" -> 10)
    }
  }
}