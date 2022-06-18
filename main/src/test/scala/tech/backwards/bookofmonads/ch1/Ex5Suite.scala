package tech.backwards.bookofmonads.ch1

import cats.implicits._
import monocle.Lens
import monocle.macros.GenLens
import munit._

/**
 * Functor
 */
class Ex5Suite extends FunSuite {
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
}