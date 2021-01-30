package com.backwards.fp.typeclass.simulacrum

import org.scalatest.matchers.must.Matchers
import org.scalatest.wordspec.AnyWordSpec
import com.backwards.fp.typeclass.Show.ops.ShowOps

class ShowSpec extends AnyWordSpec with Matchers {
  case class Foo(foo: Int)

  case class Bar(b: Int, a: String, r: Int)

  "Show" should {
    "just work" in {
      // "bar".show mustBe "simulacrum string bar"
      "bar".show mustBe "string bar"
    }
  }
}