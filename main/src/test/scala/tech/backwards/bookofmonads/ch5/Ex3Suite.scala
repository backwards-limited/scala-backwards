package tech.backwards.bookofmonads.ch5

import cats.implicits._
import cats.{Applicative, Monad}
import munit.FunSuite

/**
 * Monad Laws - What we call laws, mathematicians call theorems.
 *
 * In these tests we simplify polymorphic monadic functions (f and g).
 * Ideally we would have something like the following, but usage would make the tests less readable:
 * {{{
 *   def f[F[_]: Applicative, A, B]: A => F[B]
 * }}}
 */
class Ex3Suite extends FunSuite {
  test("Left identity") {
    val a = 1

    def unit[A](a: A): Option[A] =
      Monad[Option].pure(a)

    def f[A]: A => Option[A] =
      Applicative[Option] pure

    assertEquals(
      unit(a) >>= f,
      f(a)
    )
  }

  test("Right identity") {
    val a = 1

    def unit[A](a: A): Option[A] =
      Monad[Option].pure(a)

    def f[A]: A => Option[A] =
      Applicative[Option] pure

    assertEquals(
      f(a) >>= unit,
      f(a)
    )
  }

  test("Associativity") {
    val a = 1

    def unit[A](a: A): Option[A] =
      Monad[Option].pure(a)

    def f[A]: A => Option[A] =
      Applicative[Option] pure

    def g[A]: A => Option[A] =
      Applicative[Option] pure

    assertEquals(
      unit(a) >>= f >>= g,
      unit(a) >>= (x => f(x) >>= g)
    )
  }
}