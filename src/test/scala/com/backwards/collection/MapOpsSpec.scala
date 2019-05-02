package com.backwards.collection

import org.scalatest.{MustMatchers, WordSpec}

class MapOpsSpec extends WordSpec with MustMatchers with MapOps {
  "Map ops" should {
    "convert Map to Properties" in {
      """val p: java.util.Properties = Map[String, Int]("one" -> 1, "two" -> 2)""" must compile
    }
  }
}