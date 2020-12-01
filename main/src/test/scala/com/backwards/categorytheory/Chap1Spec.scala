package com.backwards.categorytheory

import org.scalatest.matchers.must.Matchers
import org.scalatest.wordspec.AnyWordSpec

class Chap1Spec extends AnyWordSpec with Matchers {
  "Chap 1" should {
    "implement the identity function" in {
      def identity[A](a: A): A = a

      identity(1) must ===(1)
      identity("one") must ===("one")
    }

    "implement composition function - it takes two functions as arguments and returns a function that is their composition" in {
      def compose[A, B, C](f: A => B, g: B => C): A => C =
        a => g(f(a))

      val even: Int => Boolean =
        _ % 2 == 0

      val toString: Boolean => String =
        _.toString

      val fn: Int => String =
        compose(even, toString)

      fn(4) must ===("true")
      fn(5) must ===("false")
    }
  }
}