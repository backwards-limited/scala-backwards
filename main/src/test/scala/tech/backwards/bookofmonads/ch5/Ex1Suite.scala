package tech.backwards.bookofmonads.ch5

import cats.implicits.{catsSyntaxOptionId, none}
import munit.CatsEffectSuite

/**
 * Monad Laws - What we call laws, mathematicians call theorems.
 */
class Ex1Suite extends CatsEffectSuite with FunctionFixture { self =>
  test("Function left identity law") {
    val id: String => String =
      self.id[String]

    val f: Int => String =
      _.toString

    assertEquals(
      (id `.` f)(20),
      f(20)
    )
  }

  test("Function right identity law") {
    val id: Int => Int =
      self.id[Int]

    val f: Int => String =
      _.toString

    assertEquals(
      (f `.` id)(20),
      f(20)
    )
  }

  test("Function associativity") {
    type A = Int
    type B = Int
    type C = Int
    type D = Int

    val f: C => D =
      self.id[D]

    val g: B => C =
      self.id[C]

    val h: A => B =
      self.id[B]

    assertEquals(
      (f `.` (g `.` h))(1),
      ((f `.` g) `.` h)(1)
    )
  }

  test("Function composition is NOT commutative") {
    // Addition is commutative e.g.
    assertEquals(1 + 3, 3 + 1)

    // But
    // f . g DOES NOT EQUATE TO g . f
  }

  /*
  There are many ways to prove that a law holds.
  The simplest technique, called equational reasoning, works by substituting equal expressions for equal expressions in tiny steps, until we obtain the equality we want.
  We follow a style in which every eq sign includes the reason for the equality between the two expressions surrounding it.
  For example, the proof of function associativity looks like this:

    (f . (g . h)) x
  eq -- by definition of (.)
    f ((g . h) x)
  eq -- by definition of (.)
    f (g (h x))
  eq -- by definition of (.)
    (f . g) (h x)
  eq -- by definition of (.)
    ((f . g) . h) x
  */
}

trait FunctionFixture { self =>
  // Instead of using function def identity[A](x: A): A = x, we'll define our own:
  def id[A](a: A): A = a

  // Compose - Haskell uses "." so we'll do the same... kind of:
  def `.`[A, B, C]: (B => C) => (A => B) => A => C =
    f => g => a => f(g(a))

  implicit class FunctionSyntax[B, C](f: B => C) {
    def `.`[A](g: A => B): A => C =
      (self `.` f)(g)
  }
}