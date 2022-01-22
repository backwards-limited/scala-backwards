package tech.backwards.tagless.modelling

import cats.Id
import cats.implicits._
import monocle.macros.syntax.lens._
import tech.backwards.tagless.modelling.{InMemoryUserRepository, User, UserId}
import org.scalatest.matchers.must.Matchers
import org.scalatest.wordspec.AnyWordSpec

class UserRepositorySpec extends AnyWordSpec with Matchers {
  val repository = new InMemoryUserRepository

  val user = User(
    id = Some(UserId("1")),
    firstName = "Sue",
    lastName = "Jordan",
    books = List.empty
  )

  "User repository" should {
    "retrieve a user" in {
      val retrievedUser: Id[Option[User]] = for {
        _ <- repository addUser user
        userId <- user.id.pure[Id]
        retrievedUser <- repository getUser userId
      } yield retrievedUser

      retrievedUser mustBe Option(user)
    }

    "not retrieve a user because of incorrect user id" in {
      val retrievedUser: Id[Option[User]] = for {
        _ <- repository addUser user
        userId <- Option(UserId("wrong id")).pure[Id]
        retrievedUser <- repository getUser userId
      } yield retrievedUser

      retrievedUser mustBe None
    }

    "not retrieve a user because no user id" in {
      val retrievedUser: Id[Option[User]] = for {
        _ <- repository addUser user
        userId <- None.pure[Id]
        retrievedUser <- repository getUser userId
      } yield retrievedUser

      retrievedUser mustBe None
    }

    "update an user" in {
      val updatedUser: Id[Option[User]] = for {
        _ <- repository addUser user
        _ <- repository updateUser user.lens(_.firstName).set("Bob")
        userId <- user.id.pure[Id]
        updatedUser <- repository getUser userId
      } yield updatedUser

      updatedUser mustBe Option(user.lens(_.firstName).set("Bob"))
    }
  }
}