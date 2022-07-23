package tech.backwards.bookofmonads.ch5

import cats.{Functor, Semigroup}
import cats.implicits._
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
}

trait FunctionFixture { self =>
  def fmap[F[_]: Functor, A, B](f: A => B): F[A] => F[B] =
    _.map(f)

  def fmap[F[_]: Functor, A, B](Fa: F[A])(f: A => B): F[B] =
    Fa.map(f)

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