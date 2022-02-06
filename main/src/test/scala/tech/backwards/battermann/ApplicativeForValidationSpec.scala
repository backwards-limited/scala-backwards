package tech.backwards.battermann

import scala.util.{Failure, Success, Try}
import org.scalatest.matchers.must.Matchers
import org.scalatest.wordspec.AnyWordSpec
import cats._
import cats.data.Validated.{Invalid, Valid}
import cats.data._
import cats.implicits._

/**
 * [[http://blog.leifbattermann.de/2018/03/10/how-to-use-applicatives-for-validation-in-scala-and-save-much-work/#more-1377 How To Use Applicatives For Validation]]
 */
class ApplicativeForValidationSpec extends AnyWordSpec with Matchers {
  case class User(name: String, age: Int, email: String)

  trait UserValidator[F[_]] {
    def createValidUser(name: String, age: Int, email: String): F[User]
  }

  object UserValidator {
    def apply[F[_]: UserValidator]: UserValidator[F] =
      implicitly[UserValidator[F]]

    def validate[F[_]: UserValidator, E](name: String, age: Int, email: String): F[User] =
      UserValidator[F].createValidUser(name, age, email)
  }

  "Id interpreter for a UserValidator" should {
    "do no validation and just give a User" in {
      val userValidatorIdInterpreter = new UserValidator[Id] {
        def createValidUser(name: String, age: Int, email: String): Id[User] =
          User(name, age, email)
      }

      // If we want to call the validate function, we have to have an implicit instance of UserValidator[F] in scope.
      implicit val userValidatorInterpreter: UserValidator[Id] =
        userValidatorIdInterpreter

      UserValidator.validate("John", 25, "john@example.com") mustBe User("John", 25, "john@example.com")

      UserValidator.validate("John", 25, "johnn@example") mustBe User("John", 25, "johnn@example")

      UserValidator.validate("John", -1, "john@gexample") mustBe User("John", -1, "john@gexample")

      UserValidator.validate(".John", -1, "john@gexample") mustBe User(".John", -1, "john@gexample")
    }
  }

  "Applicative" should {
    "be explained" in {
      /*
      Applicative is a type class that can be mapped over. And is has two operations called `pure` and `ap`.

      `pure` is just a constructor that lifts values into an Applicative.

      `ap` or the infix version `<*>` applies a value to a function, where both the value and the function are wrapped inside the same Applicative type.

      So if F is the type of our Applicative we can apply F[A] to an F[A => B] to get an F[B].
      It works just like normal function application, only that everything happens inside the F.

      Note that the type B in F[A => B] can also be a function.
      Therefore we can continue to apply values to our F until we are left with a final result.

      Here is an example of how to apply values of type A, B, and C to the function A => B => C => D to get a D at the end:

      F[A => (B => (C => D))] <*> F[A] = F[B => (C => D)]

      F[B => (C => D)]        <*> F[B] = F[C => D]

      F[C => D]               <*> F[C] = F[D]

      The parentheses are for clarification and can be omitted. Note that the function inside the F has to be curried.
      We can also write this in one line:

      F[A => B => C => D] <*> F[A] <*> F[B] <*> F[C] = F[D]
      */
    }

    "show that Option has an Applicative - uncurried function" in {
      val add: (Int, Int) => Int =
        _ + _

      val result: Option[Int] =
        add.curried.pure[Option] <*> Option(2) <*> Option(5)

      result mustBe Some(7)
    }

    "show that Option has an Applicative - curried function" in {
      val add: Int => Int => Int =
        a => b => a + b

      val result: Option[Int] =
        add.pure[Option] <*> Option(2) <*> Option(5)

      result mustBe Some(7)
    }

    """be used for validation logic, but not commit to to a specific applicative (such as Option)
      |and instead have contraint on abstract type F to have instance of ApplicativeError, a special kind of Applicative with additional error type and additional operations""".stripMargin in {
      // First we create a custom error type for the user validation:
      sealed trait UserValidationError

      case object NameNotValid extends UserValidationError

      case object AgeOutOfRange extends UserValidationError

      case object EmailNotValid extends UserValidationError

      /*
      The applicative instances from Cats have different error types.
      Some have Throwable or Unit. Others have generic error types. And yet others have generic error types with constraints.

      Therefore we have to tell our generic validator how to construct an abstract error type from the concrete type that we just defined.
      We do this by passing a function UserValidationError => E to the UserValidator constructor.
      */

      def userValidator[F[_]: ApplicativeError[*[_], E], E](mkError: UserValidationError => E): UserValidator[F] =
        new UserValidator[F] {
          def validateName(name: String): F[String] =
            if (name.matches("(?i:^[a-z][a-z ,.'-]*$)")) name.pure[F]
            else ApplicativeError[F, E].raiseError(mkError(NameNotValid))

          def validateAge(age: Int): F[Int] =
            if (age >= 18 && age < 120) age.pure[F]
            else ApplicativeError[F, E].raiseError(mkError(AgeOutOfRange))

          def validateEmail(email: String): F[String] =
            if (email.matches("^[a-zA-Z0-9._%+-]+@[a-zA-Z0-9.-]+\\.[a-zA-Z]{2,}$")) email.pure[F]
            else ApplicativeError[F, E].raiseError(mkError(EmailNotValid))

          def createValidUser(name: String, age: Int, email: String): F[User] =
            User.curried.pure[F] <*> validateName(name) <*> validateAge(age) <*> validateEmail(email)
        }

      // Some examples for Option, Try, Either:

      val userValidatorOptionInterpreter: UserValidator[Option] =
        userValidator[Option, Unit](_ => ())

      val userValidatorTryInterpreter: UserValidator[Try] =
        userValidator[Try, Throwable](err => new Throwable(err.toString))

      val userValidatorEitherInterpreter: UserValidator[UserValidationError Either *] =
        userValidator[UserValidationError Either *, UserValidationError](identity)

      // Bring in an implicit validator into scope:
      {
        implicit val userValidatorInterpreter: UserValidator[Try] = userValidatorTryInterpreter

        UserValidator.validate("John", 25, "john@example.com") mustBe Success(User("John", 25, "john@example.com"))

        val Failure(t1) = UserValidator.validate("John", 25, "johnn@example")
        t1.getMessage mustBe EmailNotValid.toString

        val Failure(t2) = UserValidator.validate("John", -1, "john@gexample")
        t2.getMessage mustBe AgeOutOfRange.toString

        val Failure(t3) = UserValidator.validate(".John", -1, "john@gexample")
        t3.getMessage mustBe NameNotValid.toString
      }

      // If we want to collect error messages we can use the type Validated from Cats.
      {
        val userValidatorValidatedInterpreter: UserValidator[NonEmptyList[UserValidationError] Validated *] =
          userValidator[NonEmptyList[UserValidationError] Validated *, NonEmptyList[UserValidationError]](NonEmptyList.one)

        implicit val userValidatorInterpreter: UserValidator[NonEmptyList[UserValidationError] Validated *] = userValidatorValidatedInterpreter

        UserValidator.validate("John", 25, "john@example.com") mustBe Valid(User("John", 25, "john@example.com"))

        UserValidator.validate("John", 25, "johnn@example") mustBe Invalid(NonEmptyList.one(EmailNotValid))

        UserValidator.validate("John", -1, "john@gexample") mustBe Invalid(NonEmptyList.of(AgeOutOfRange, EmailNotValid))

        UserValidator.validate(".John", -1, "john@gexample") mustBe Invalid(NonEmptyList.of(NameNotValid, AgeOutOfRange, EmailNotValid))
      }
    }
  }
}