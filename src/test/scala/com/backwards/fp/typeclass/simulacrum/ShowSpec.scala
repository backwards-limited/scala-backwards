package com.backwards.fp.typeclass.simulacrum

import org.scalatest._
import org.scalatest.wordspec.AnyWordSpec
import com.backwards.fp.typeclass.simulacrum.Show.ops._

class ShowSpec extends AnyWordSpec with MustMatchers {
  case class Foo(foo: Int)

  case class Bar(b: Int, a: String, r: Int)

  "Show" should {
    "just work" in {
      "bar".show mustBe "simulacrum string bar"
    }
  }
}