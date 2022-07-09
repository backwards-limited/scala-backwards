package tech.backwards.bookofmonads.ch3

import cats.implicits.{catsSyntaxOptionId, none}
import munit._

/**
 * Applicatives - Tuples before Monodial Functor
 */
class Ex4Suite extends FunSuite {
  test("Nest triple") {
    def nest[A, B, C](t: (A, B, C)): (A, (B, C)) = {
      val (a, b, c) = t

      (a, (b, c))
    }

    assertEquals(
      nest(5, 5.0, "5"),
      (5, (5.0, "5"))
    )
  }

  test("Nest quadruple") {
    def nest[A, B, C, D](q: (A, B, C, D)): (A, (B, (C, D))) = {
      val (a, b, c, d) = q

      (a, (b, (c, d)))
    }

    assertEquals(
      nest(5, 5.0, "5", true),
      (5, (5.0, ("5", true)))
    )
  }
}