package tech.backwards.bookofmonads.ch3

import cats.implicits.{catsSyntaxOptionId, none}
import munit._

/**
 * Applicatives.
 */
class Ex2Suite extends FunSuite {
  test("Applicative") {
    import ApplicativeFixture._

    implicit object ApplicativeOption extends Applicative[Option] {
      def point[A](x: A): Option[A] =
        x.some

      def ap[A, B](x: Option[A])(f: Option[A => B]): Option[B] =
        (x, f) match {
          case (Some(a), Some(fab)) =>
            point(fab(a))

          case _ =>
            None
        }
    }

    val f: Int => String => Boolean =
      i => s => true

    assertEquals(
      Applicative[Option].ap("1".some)(Applicative[Option].map(1.some)(f)),
      true.some
    )
  }

  object FunctorFixture {
    trait Functor[F[_]] {
      def map[A, B](x: F[A])(f: A => B): F[B]
    }

    object Functor {
      def apply[F[_]: Functor]: Functor[F] = implicitly
    }
  }

  object ApplicativeFixture {
    import FunctorFixture._

    trait Applicative[F[_]] extends Functor[F] {
      def point[A](x: A): F[A]

      // def ap[A, B]: F[A => B] => F[A] => F[B]
      def ap[A, B](x: F[A])(f: F[A => B]): F[B]

      def map[A, B](x: F[A])(f: A => B): F[B] =
        ap(x)(point(f))
    }

    object Applicative {
      def apply[F[_]: Applicative]: Applicative[F] = implicitly
    }
  }
}