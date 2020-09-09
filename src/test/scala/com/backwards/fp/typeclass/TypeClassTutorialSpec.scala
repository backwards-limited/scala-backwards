package com.backwards.fp.typeclass

import org.scalatest.matchers.must.Matchers
import org.scalatest.wordspec.AnyWordSpec

class TypeClassTutorialSpec extends AnyWordSpec with Matchers {
  // Trait with generic parameters
  trait Formatter[A] {
    def fmt(a: A): String
  }

  object Formatter {
    def apply[A: Formatter]: Formatter[A] = implicitly

    def fmt[A: Formatter](a: A): String =
      Formatter[A].fmt(a)

    object syntax {
      implicit class FromatterOps[A: Formatter](a: A) {
        def fmt: String = Formatter[A].fmt(a)
      }
    }

    // Implicit instances with [concrete] types
    object instances {
      implicit val float: Formatter[Float] =
        (a: Float) => s"${a}f"

      implicit val boolean: Formatter[Boolean] =
        (a: Boolean) => a.toString.toUpperCase

      implicit def list[A: Formatter]: Formatter[List[A]] =
        (a: List[A]) => a.map(e => Formatter[A].fmt(e)).mkString(" :: ")
    }
  }

  "Type class" should {
    "be successfully used" in {
      // Important: importing implementations
      import Formatter.instances._
      import Formatter.syntax._

      val floats = List(4.5f, 1f)

      Formatter.fmt(floats) mustBe "4.5f :: 1.0f"
      floats.fmt mustBe "4.5f :: 1.0f"

      val booleans = List(true, false)

      Formatter.fmt(booleans) mustBe "TRUE :: FALSE"
    }

    "fail compilation for no appropriate implementation" in {
      val integers = List(1, 2, 3)

      // Won't compile with something like: could not find implicit value for parameter ev: Formatter[List[Int]]
      "Formatter.fmt(integers)" mustNot compile
    }
  }
}