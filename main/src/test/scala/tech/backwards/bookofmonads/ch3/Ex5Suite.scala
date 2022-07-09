package tech.backwards.bookofmonads.ch3

import cats.implicits.{catsSyntaxOptionId, none}
import munit._

/**
 * Monoidal Functor
 */
class Ex5Suite extends FunSuite {
  test("Monoidal Functor for Option") {
    import MonoidalFunctorFixture._

    implicit val monoidalFunctorOption: MonoidalFunctor[Option] =
      new MonoidalFunctor[Option] {
        def unit: Option[Unit] =
          ().some

        def tupled[A, B](Fa: Option[A], Fb: Option[B]): Option[(A, B)] =
          Fa zip Fb
      }

    assertEquals(
      MonoidalFunctor[Option].unit,
      ().some
    )

    assertEquals(
      MonoidalFunctor[Option].tupled(5.some, "5".some),
      (5, "5").some
    )
  }

  /**
   * Amazingly, applicative and monoidal functors define the same notion!
   * We can see it by providing an implementation of Applicative in terms of Monoidal and vice versa:
   */
  test("Applicative in terms of Monoidal Functor") {
    import MonoidalFunctorFixture._

    /**
     * Functor
     */
    trait Functor[F[_]] {
      def map[A, B](Fa: F[A])(f: A => B): F[B]
    }

    object Functor {
      def apply[F[_]: Functor]: Functor[F] = implicitly
    }

    /**
     * Applicative in terms of Monoidal Functor
     */
    trait Applicative[F[_]] extends Functor[F] with MonoidalFunctor[F] {
      def point[A](a: A): F[A] =
        map(unit)(_ => a)

      def ap[A, B](Fa: F[A])(Ff: F[A => B]): F[B] =
        map(tupled(Ff, Fa)) { case (f, a) => f(a)}
    }

    object Applicative {
      def apply[F[_]: Applicative]: Applicative[F] = implicitly
    }

    implicit val applicativeOption: Applicative[Option] =
      new Applicative[Option] {
        def unit: Option[Unit] =
          ().some

        def tupled[A, B](Fa: Option[A], Fb: Option[B]): Option[(A, B)] =
          Fa zip Fb

        def map[A, B](Fa: Option[A])(f: A => B): Option[B] =
          Fa map f
      }

    assertEquals(
      Applicative[Option].ap(2.some)(((x: Int) => x + 3).some),
      5.some
    )
  }

  test("Applicative in terms of Monoidal Functor") {
    import MonoidalFunctorFixture._

    /**
     * Functor
     */
    trait Functor[F[_]] {
      def map[A, B](Fa: F[A])(f: A => B): F[B]
    }

    object Functor {
      def apply[F[_]: Functor]: Functor[F] = implicitly
    }

    /**
     * Monoidal Functor in terms of Applicative. Why go in the other direction as well?
     *
     * Presenting a concept in two different ways might seem at first like a beautiful academic excursion but without much practical value.
     * Not in this case: the idea of an applicative as a structure that is able to lift tuples lies beneath Scala’s applicative builder operator: |@| — also known as the “scream operator.”
     * Here’s an example:
     * {{{
     *   (x1 |@| ... |@| xN)(f)
     *   // If the last argument is a function, parentheses may be omitted
     *   (x1 |@| x2) { _ + _ }
     *   // With the help of Scala's notation for partial application
     * }}}
     */
    trait Applicative[F[_]] extends Functor[F] with MonoidalFunctor[F] {
      def point[A](a: A): F[A]

      def ap[A, B](Fa: F[A])(Ff: F[A => B]): F[B]

      def unit: F[Unit] =
        point(())

      /* Explicit
      def tupled[A, B](Fa: F[A], Fb: F[B]): F[(A, B)] = {
        val Ff: F[B => (A, B)] =
          map(Fa)(a => (b: B) => (a, b))

        ap(Fb)(Ff)
      }
      */

      def tupled[A, B](Fa: F[A], Fb: F[B]): F[(A, B)] =
        // Less explicit, though I though this would work: ap(Fb)(map(Fa)((_: A, _: B).curried))
        ap(Fb)(map(Fa)(a => b => (a, b)))
    }

    object Applicative {
      def apply[F[_]: Applicative]: Applicative[F] = implicitly
    }

    implicit val applicativeOption: Applicative[Option] =
      new Applicative[Option] {
        def point[A](a: A): Option[A] =
          a.some

        def ap[A, B](Fa: Option[A])(Ff: Option[A => B]): Option[B] =
          Fa.flatMap(a => Ff.map(_(a)))

        def map[A, B](Fa: Option[A])(f: A => B): Option[B] =
          ap(Fa)(point(f))
      }

    assertEquals(
      Applicative[Option].ap(2.some)(((x: Int) => x + 3).some),
      5.some
    )
  }

  object MonoidalFunctorFixture {
    trait MonoidalFunctor[F[_]] {
      def unit: F[Unit]

      def tupled[A, B](Fa: F[A], Fb: F[B]): F[(A, B)]
    }

    object MonoidalFunctor {
      def apply[F[_]: MonoidalFunctor]: MonoidalFunctor[F] = implicitly
    }
  }
}