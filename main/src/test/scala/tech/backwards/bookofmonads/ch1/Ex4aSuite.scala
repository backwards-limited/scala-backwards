package tech.backwards.bookofmonads.ch1

import cats.implicits._
import monocle.Lens
import monocle.macros.GenLens
import munit._

/**
 * List
 */
class Ex4aSuite extends FunSuite {
  type Name = String
  type Age = Int

  case class Person(name: Name, age: Age)

  val ageL: Lens[Person, Age] =
    GenLens[Person](_.age)

  test("List flatMap as flatten (or concat when specifically referring to List) map") {
    def concat[A](xs: List[List[A]]): List[A] =
      xs match {
        case xs :: ys => xs ::: concat(ys)
        case _ => Nil
      }

    def map[A, B](f: A => B, xs: List[A]): List[B] =
      xs match {
        case x :: xs => f(x) :: map(f, xs)
        case Nil => Nil
      }

    def flatMap[A, B](xs: List[A], g: A => List[B]): List[B] =
      concat(map(g, xs))

    assertEquals(
      concat(List(List(Person("Scooby", 5), Person("Scooby", 6)), List(Person("Scooby", 7)))),
      List(Person("Scooby", 5), Person("Scooby", 6), Person("Scooby", 7))
    )

    assertEquals(
      map(ageL.modify(_ * 2), List(Person("Scooby", 5), Person("Scooby", 6))),
      List(Person("Scooby", 10), Person("Scooby", 12))
    )

    assertEquals(
      flatMap(List(Person("Scooby", 5), Person("Scooby", 6)), (person: Person) => List(ageL.modify(_ * 2)(person))),
      List(Person("Scooby", 10), Person("Scooby", 12))
    )
  }

  /*
  And at this moment in time, we have enough information to formulate Monad.
  State[S, A], Option[A], List[A] share commonalities we describe within Monad.

  - singleton operation:
    - Haskell
      - return
      - pure
    - Scala
      - point
      - pure
      - unit

  - sequence operation:
    - Haskell
      - bind (>>=)
    - Scala
      - flatMap

  - flatten operation:
    - Haskell
      - join
    - Scala
      - flatten

  The following definitions of Monad show that Monads are a "higher kinded" abstraction.

  Being a monad is not a property of a concrete type (like Int or Bool) but of a type constructor (like Maybe or List).
  (Hence we can define MonadList extends Monad[List] as List is a type constructor).
  Haskell’s syntax hides this subtle point, but Scala is explicit:
  we need to write M[_] to tell the compiler that members of this interface have a “type hole” that may vary in its different methods.
  */

  test("Monad as a context") {
    /*
    class Monad m where
      return :: a -> m a

      (>>=)  :: m a -> (a -> m b) -> m b
    */

    trait Monad[M[_]] {
      def point[A](x: A): M[A]

      def bind[A, B](x: M[A])(f: A => M[B]): M[B]
    }

    object MonadList extends Monad[List] {
      def point[A](x: A): List[A] =
        List(x)

      def bind[A, B](xs: List[A])(f: A => List[B]): List[B] =
        xs match {
          case x :: xs => f(x) ::: bind(xs)(f)
          case Nil => Nil
        }
    }

    assertEquals(
      MonadList.bind(List(Person("Scooby", 5), Person("Scooby", 6)))(ageL.modify(_ * 2).andThen(_.pure[List])),
      List(Person("Scooby", 10), Person("Scooby", 12))
    )
  }

  test("Monad as a box") {
    /*
    class Monad m where
      return :: a -> m a

      fmap   :: (a -> b) -> m a -> m b

      join   :: m (m a) -> m a
    */

    trait Monad[M[_]] {
      def point[A](x: A): M[A]

      def map[A, B](x: M[A])(f: A => B): M[B]

      def join[A](xx: M[M[A]]): M[A]
    }

    object MonadList extends Monad[List] {
      def point[A](x: A): List[A] =
        List(x)

      def map[A, B](xs: List[A])(f: A => B): List[B] =
        xs map f

      def join[A](xxs: List[List[A]]): List[A] =
        xxs match {
          case x :: xs => x ::: join(xs)
          case _ => Nil
        }
    }

    assertEquals(
      MonadList.join(
        MonadList.map(List(Person("Scooby", 5), Person("Scooby", 6)))(ageL.modify(_ * 2).andThen(_.pure[List]))
      ),
      List(Person("Scooby", 10), Person("Scooby", 12))
    )
  }

  test("Monad as a context for Option reusing andThen") {
    implicit class OptionExtension[A](x: Option[A]) {
      def andThen[B](f: A => Option[B]): Option[B] =
        x match {
          case None => None
          case Some(x) => f(x)
        }
    }

    trait Monad[M[_]] {
      def point[A](x: A): M[A]

      def bind[A, B](x: M[A])(f: A => M[B]): M[B]
    }

    object MonadOption extends Monad[Option] {
      def point[A](x: A): Option[A] =
        x.some

      def bind[A, B](x: Option[A])(f: A => Option[B]): Option[B] =
        x match {
          case Some(x) => f(x)
          case None => none
        }
    }

    assertEquals(
      MonadOption.bind(Person("Scooby", 5).some)(ageL.modify(_ * 2).andThen(_.pure[Option])),
      Person("Scooby", 10).some
    )
  }
}