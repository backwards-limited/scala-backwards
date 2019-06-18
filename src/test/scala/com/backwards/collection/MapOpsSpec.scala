package com.backwards.collection

import org.scalatest.{MustMatchers, WordSpec}

class MapOpsSpec extends WordSpec with MustMatchers with MapOps {
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