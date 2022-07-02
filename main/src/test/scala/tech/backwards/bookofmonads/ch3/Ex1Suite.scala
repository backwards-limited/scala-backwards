package tech.backwards.bookofmonads.ch3

import cats.implicits.{catsSyntaxOptionId, none}
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
    import cats.Monad

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

  /**
   * After lift2, we could have lift3, lift4.... liftN:
   * {{{
   *   def liftN[M[_]: Monad, A, B, ... N](f: (A, B, ... N => C): (M[A], M[B], ... M[N]) => M[C]
   * }}}
   *
   * But instead of coding an infinite number of "lift" functions, we only need 1.
   *
   * "map" expects a unary function. Let's first handle a binary (next we would handle tertiary, all the way up to N):
   * {{{
   *   def map[M[_], A, B](f: A => B): M[A] => M[B]
   *
   *   // Our binary function:
   *   def g[A, B, C]: A => B => C
   *
   *   // You can think of "g" as unary where the input is "A" and the output is "A => B" (instead of just "B") - So its output is just a function instead of a value.
   *
   *   map(g): M[A] => M[B => C]
   *
   *   // Let's say that:
   *   x: M[A]
   *
   *   // So:
   *   map(g)(x): M[B => C]
   *
   *   // Ok, so now we have M[B => C], we need a new function, which will be called "ap".
   *   // map is:
   *   // Given a function A => B, apply it to M[A] => M[B]
   *   // where as
   *   // ap is:
   *   // Given a function M[A => B], apply it to M[A] => M[B]
   *   // or following from above:
   *   // Given a function M[B => C], apply it to M[B] => M[C]
   * }}}
   */
  test("ap") {
    import MonadFixture._

    def apVersion1[M[_]: Monad, B, C]: M[B => C] => M[B] => M[C] =
      mbc => mb => {
        implicitly[Monad[M]].bind(mb)(b =>
          implicitly[Monad[M]].bind(mbc)(bc =>
            implicitly[Monad[M]].point(bc(b))
          )
        )
      }

    def apVersion2[M[_]: Monad, B, C]: M[B => C] => M[B] => M[C] =
      mbc => mb => {
        Monad[M].bind(mb)(b =>
          Monad[M].bind(mbc)(bc =>
            Monad[M].point(bc(b))
          )
        )
      }

    def ap[M[_]: Monad, B, C]: M[B => C] => M[B] => M[C] =
      mbc => mb => {
        mb.bind(b =>
          mbc.bind(bc =>
            bc(b).point[M]
          )
        )
      }
  }

  /**
   * Now we need nothing more than "map" and "ap" to be able to lift any pure function regardless of the number of arguments.
   * {{{
   *    // Take a function with 3 arguments:
   *    f :: a -> b -> c -> d
   *
   *    // to apply in a monadic context i.e. we want to apply it to the values:
   *    x :: m a,   y :: m b,  z :: m c
   *
   *    // The first step is to "map":
   *    map(f)(x) // Gives m (b -> c -> d), because "map" maps the first argument and the return just happens to be a function.
   *
   *    // Now with a function inside our context we use "ap":
   *    ap(map(f)(x))(y) // Gives m (c -> d)
   *
   *    // And once more:
   *    ap(ap(map(f)(x))(y))(z) // Gives m d
   * }}}
   */
  test("map and ap") {
    import ApplicativeFixture._

    implicit object ApplicativeOption extends Applicative[Option] {
      def point[A](x: A): Option[A] =
        x.some

      def map[A, B](x: Option[A])(f: A => B): Option[B] =
        bind(x)(f.andThen(_.some))

      def bind[A, B](x: Option[A])(f: A => Option[B]): Option[B] =
        x match {
          case Some(x) => f(x)
          case None => none
        }

      def ap[A, B]: Option[A => B] => Option[A] => Option[B] =
        mab => ma =>
          bind(ma)(a =>
            bind(mab)(ab =>
              point(ab(a))
            )
          )
    }

    val f: Int => String => Double => Boolean =
      i => s => d => true

    val x: Option[Int] =
      2.some

    val y: Option[String] =
      "2".some

    val z: Option[Double] =
      2.0.some

    val step1: Option[String => Double => Boolean] =
      Applicative[Option].map(x)(f)

    val step2: Option[Double => Boolean] =
      Applicative[Option].ap(step1)(y)

    val step3: Option[Boolean] =
      Applicative[Option].ap(step2)(z)

    assertEquals(step3, true.some)

    val App = Applicative[Option]
    import App.ap
    import App.map

    assertEquals(
      ap(ap(map(x)(f))(y))(z),
      true.some
    )
  }

  object MonadFixture {
    trait Monad[M[_]] {
      def point[A](x: A): M[A]

      def map[A, B](x: M[A])(f: A => B): M[B]

      def bind[A, B](x: M[A])(f: A => M[B]): M[B]
    }

    object Monad {
      def apply[M[_]: Monad]: Monad[M] = implicitly
    }

    implicit class MonadOps[M[_]: Monad, A](m: M[A]) {
      def bind[B](f: A => M[B]): M[B] =
        Monad[M].bind(m)(f)
    }

    implicit class MonadLift[A](a: A) {
      def point[M[_]: Monad]: M[A] =
        Monad[M].point(a)
    }
  }

  object ApplicativeFixture {
    import MonadFixture._

    trait Applicative[M[_]] extends Monad[M] {
      def ap[A, B]: M[A => B] => M[A] => M[B]
    }

    object Applicative {
      def apply[M[_]: Applicative]: Applicative[M] = implicitly
    }
  }
}