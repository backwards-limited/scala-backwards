package tech.backwards.bookofmonads.ch1

import cats.implicits._
import monocle.Lens
import monocle.macros.GenLens
import munit._

/**
 * Option
 */
class Ex4Suite extends FunSuite {
  type Name = String
  type Age = Int

  final case class Person(name: Name, age: Age)

  val ageL: Lens[Person, Age] =
    GenLens[Person](_.age)

  val validateName: Name => Option[Name] =
    name => if (name.filter(_.isLetter) == name) name.some else none

  val validateAge: Age => Option[Age] =
    age => if (age > 0 && age < 110) age.some else none

  test("Option with awkward pattern matching") {
    def validatePerson(name: Name, age: Age): Option[Person] =
      validateName(name) match {
        case None =>
          none

        case Some(name) =>
          validateAge(age) match {
            case None =>
              none

            case Some(age) =>
              Person(name, age).some
          }
      }

    assertEquals(validatePerson("Scooby", 5), Person("Scooby", 5).some)
    assertEquals(validatePerson("Scooby1", 5), none)
    assertEquals(validatePerson("Scooby", -5), none)
    assertEquals(validatePerson("Scooby1", -5), none)
  }

  test("Option using andThen version 1") {
    def andThen[A, B](x: Option[A])(f: A => Option[B]): Option[B] =
      x match {
        case None => None
        case Some(x) => f(x)
      }

    def validatePerson(name: Name, age: Age): Option[Person] =
      andThen(validateName(name))(name =>
        andThen(validateAge(age))(age =>
          Person(name, age).some
        )
      )

    assertEquals(validatePerson("Scooby", 5), Person("Scooby", 5).some)
    assertEquals(validatePerson("Scooby1", 5), none)
    assertEquals(validatePerson("Scooby", -5), none)
    assertEquals(validatePerson("Scooby1", -5), none)
  }

  test("Option using andThen version 2") {
    implicit class OptionExtension[A](x: Option[A]) {
      def andThen[B](f: A => Option[B]): Option[B] =
        x match {
          case None => None
          case Some(x) => f(x)
        }
    }

    def validatePerson(name: Name, age: Age): Option[Person] =
      validateName(name) andThen (name =>
        validateAge(age) andThen (age => Person(name, age).some)
      )

    assertEquals(validatePerson("Scooby", 5), Person("Scooby", 5).some)
    assertEquals(validatePerson("Scooby1", 5), none)
    assertEquals(validatePerson("Scooby", -5), none)
    assertEquals(validatePerson("Scooby1", -5), none)
  }

  /*
  We have the following similarity:

  def    next[S, A, B](s: State[S, A], f: A => State[S, B]): State[S, B]

  def andThen[A,    B](o: Option  [A], f: A => Option[  B]): Option[  B]

  They look fairly similar, right?
  In some sense, being optional is also a context, but instead of adding the ability to consume and modify a state, it allows failure with no value, in addition to returning a value.
  In a similar fashion, andThen sequences computations, each of them possibly failing, into one complete computation.
  */

  test("Option with map") {
    def map[A, B](f: A => B, o: Option[A]): Option[B] =
      o match {
        case None => None
        case Some(x) => Some(f(x))
      }

    assertEquals(
      map(ageL.modify(_ + 1), Person("Scooby", 5).some),
      Person("Scooby", 6).some
    )
  }

  test("Option singleton") {
    def singleton[A](a: A): Option[A] =
      a.some

    assertEquals(
      singleton(Person("Scooby", 5)),
      Person("Scooby", 5).some
    )
  }

  test("Option flatten") {
    def flatten[A](o: Option[Option[A]]): Option[A] =
      o match {
        case Some(Some(x)) => x.some
        case _ => None
      }

    assertEquals(
      flatten(Person("Scooby", 5).some.some),
      Person("Scooby", 5).some
    )
  }
}