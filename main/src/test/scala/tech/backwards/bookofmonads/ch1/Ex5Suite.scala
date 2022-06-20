package tech.backwards.bookofmonads.ch1

import cats.implicits._
import monocle.Lens
import monocle.macros.GenLens
import munit._

/**
 * Functor - a short digression
 */
class Ex5Suite extends FunSuite {
  type Name = String
  type Age = Int

  final case class Person(name: Name, age: Age)

  val ageL: Lens[Person, Age] =
    GenLens[Person](_.age)

  test("Functor") {
    trait Functor[F[_]] {
      def map[A, B](x: F[A])(f: A => B): F[B]
    }

    implicit object FunctorOption extends Functor[Option] {
      def map[A, B](x: Option[A])(f: A => B): Option[B] =
        x match {
          case Some(x) => f(x).some
          case _ => none
        }
    }

    assertEquals(
      FunctorOption.map(Person("Scooby", 5).some)(ageL.modify(_ * 2)),
      Person("Scooby", 10).some
    )

    assertEquals(
      FunctorOption.map(none[Person])(ageL.modify(_ * 2)),
      none
    )

    def fmap[F[_]: Functor, A, B](f: A => B): F[A] => F[B] =
      implicitly[Functor[F]].map(_)(f)

    assertEquals(
      fmap(ageL.modify(_ * 2)).apply(Person("Scooby", 5).some),
      Person("Scooby", 10).some
    )
  }
}