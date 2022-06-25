package tech.backwards.bookofmonads.ch3

import cats.Monad
import cats.implicits.catsSyntaxOptionId
import cats.syntax.flatMap.toFlatMapOps
import cats.syntax.functor.toFunctorOps
import munit._

/**
 * Lifting pure functions.
 *
 * The best-known solution to the problem of embedding pure computations into monads: lifting.
 * This solution led to the discovery of a close relative of monads: applicative functors, or applicatives, for short.
 *
 * Lift2, Lift3... Ap
 */
class Ex1Suite extends FunSuite {
  /**
   * {{{
   *  class Functor f where
   *    fmap :: (a -> b) -> f a -> f b
   *
   *  trait Functor[F[_]] {
   *    def map[A, B](x: F[A])(f: A => B): F[B]
   *  }
   *
   *  // Reorder and parenthesis:
   *  (a -> b) -> (f a -> f b)
   *
   *  (A => B) => (F[A] => F[B])
   *
   *  // We can say that the function is lifted into a functor.
   * }}}
   *
   * The power of Functor is rather limited, though:
   * the set of functions that can be lifted using fmap is restricted to those that take one argument.
   * Alas, many interesting functions take several arguments.
   * Some primary examples are arithmetic operations, which are usually binary.
   * Being more concrete, we cannot lift the addition operation (+) to work on Maybe Int or Option[Int] values with the method provided by Functor.
   *
   * Writing a lifted version of addition using monads, however, is straightforward:
   */
  test("Monad lifting is better than Functor lifting") {
    def plus(x: Option[Int], y: Option[Int]): Option[Int] =
      for {
        a <- x
        b <- y
      } yield a + b

    assertEquals(
      plus(2.some, 3.some),
      5.some
    )
  }

  test("Abstract the above idea into a generic lift2 function") {
    def lift2[M[_]: Monad, A, B, C](f: (A, B) => C): (M[A], M[B]) => M[C] =
      (x, y) =>
        for {
          a <- x
          b <- y
        } yield f(a, b)

    def plus(x: Int, y: Int): Int =
      x + y

    assertEquals(
      lift2[Option, Int, Int, Int](plus).apply(2.some, 3.some),
      5.some
    )
  }
}