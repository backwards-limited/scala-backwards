/*
package com.backwards.fp.pancakes

import io.circe.literal.JsonStringContext
import io.circe.syntax.EncoderOps
import org.scalatest.matchers.must.Matchers
import org.scalatest.wordspec.AnyWordSpec

/**
 * [[https://blog.softwaremill.com/functional-pancakes-cf70023f0dcb Functional pancakes]]
 */
class PancakeSpec extends AnyWordSpec with Matchers {
  "ADT" should {
    "be encoded to JSON" in {
      import io.circe.generic.auto._

      json"""{
        "ingredient": {
          "Flour": {
            "grams": 6
          }
        }
      }""" mustBe IngredientReceived(Flour(6)).asJson
    }

    "be encoded to JSON with descriminator" in new Json {
      import io.circe.generic.auto._

      json"""{
        "ingredient": {
          "ingredient": "flour",
          "grams": 6
        }
      }""" mustBe IngredientReceived(Flour(6)).asJson
    }
  }
}*/
