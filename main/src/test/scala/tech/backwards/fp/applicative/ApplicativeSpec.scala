package tech.backwards.fp.applicative

import scala.util.Try
import org.scalatest.matchers.must.Matchers
import org.scalatest.wordspec.AnyWordSpec

/**
 * [[http://blog.leifbattermann.de/2018/03/10/how-to-use-applicatives-for-validation-in-scala-and-save-much-work/ How To Use Applicatives For Validation]]
 */
class ApplicativeSpec extends AnyWordSpec with Matchers {
  final case class User(name: String, age: Int, email: String)

  trait UserValidator[F[_]] {
    def createValidUser(name: String, age: Int, email: String): F[User]
  }

  object UserValidator {
    def apply[F[_]: UserValidator]: UserValidator[F] =
      implicitly

    def validate[F[_]: UserValidator, E](name: String, age: Int, email: String): F[User] =
      UserValidator[F].createValidUser(name, age, email)
  }

  "Id interpreter" should {
    "perform no validation, simply construct a User instance" in {
      import cats._
      import cats.implicits._

      val userValidatorIdInterpreter = new UserValidator[Id] {
        def createValidUser(name: String, age: Int, email: String): Id[User] =
          User(name, age, email)
      }

      // If we want to call the validate function, we have to have an implicit instance of UserValidator[F] in the scope.
      implicit val userValidator: UserValidator[Id] =
        userValidatorIdInterpreter

      println(UserValidator.validate("John", 25, "john@example.com"))

      // As we see, no validation is performed (age is -1)
      println(UserValidator.validate("John", -1, "john@gexample"))
    }
  }

  /**
   * Applicative is a type class that can be mapped over. And is has two operations called pure and ap.
   * ap or the infix version <*> applies a value to a function, where both the value and the function are wrapped inside the same Applicative type.
   * So if F is the type of our Applicative we can apply F[A] to an F[A => B] to get an F[B].
   * Note that the type B in F[A => B] can also be a function. Therefore we can continue to apply values to our F until we are left with a final result.
   *
   * Here is an example of how to apply values of type A, B, and C to the function A => B => C => D to get a D at the end:
   * {{{
   *  F[A => (B => (C => D))] <*> F[A] = F[B => (C => D)]
   *
   *  F[B => (C => D)]        <*> F[B] = F[C => D]
   *
   *  F[C => D]               <*> F[C] = F[D]
   * }}}
   * The parentheses are for clarification and can be omitted. Note that the function inside the F has to be curried.
   * We can also write this in one line:
   * {{{
   *  F[A => B => C => D] <*> F[A] <*> F[B] <*> F[C] = F[D]
   * }}}
   */
  "Option" should {
    "have an applicative instance - curried function" in {
      import cats.implicits._

      val add: Int => Int => Int =
        x => y => x + y

      val result: Option[Int] =
        add.pure[Option] <*> Option(2) <*> Option(5)

      result mustBe Option(7)
    }

    "have an applicative instance - alternative curried function - map and then ap" in {
      import cats.implicits._

      val add: Int => Int => Int =
        x => y => x + y

      val result: Option[Int] =
        Option(2) map add ap Option(5)

      result mustBe Option(7)
    }

    "have an applicative instance - non-curried function" in {
      import cats.implicits._

      val add: (Int, Int) => Int =
        _ + _

      val result: Option[Int] =
        add.curried.pure[Option] <*> Option(2) <*> Option(5)

      result mustBe Option(7)
    }

    "have an applicative instance - explicit" in {
      import cats.implicits._

      val add: Int => Int => Int =
        x => y => x + y

      val partialResult: Option[Int => Int] =
        add.pure[Option] <*> Option(2)

      val result: Option[Int] =
        partialResult <*> Option(5)

      result mustBe Option(7)
    }

    "have an applicative instance - explicit defining our own applicative instance" in {
      import cats.implicits._
      import cats._

      implicit val myOptionApplicative: Applicative[Option] =
        new Applicative[Option] {
          def pure[A](x: A): Option[A] = Some(x)

          def ap[A, B](ff: Option[A => B])(fa: Option[A]): Option[B] =
            // we are avoiding flatMap/map to avoid allocating more closures
            if (ff.isDefined && fa.isDefined) Some(ff.get(fa.get))
            else None
        }

      val add: Int => Int => Int =
        x => y => x + y

      val partialResult: Option[Int => Int] =
        add.pure[Option] <*> Option(2)

      val result: Option[Int] =
        partialResult <*> Option(5)

      result mustBe Option(7)
    }

    "have an applicative instance - alternative explicit defining our own applicative instance" in {
      import cats.implicits._
      import cats._

      implicit val myOptionApplicative: Applicative[Option] =
        new Applicative[Option] {
          def pure[A](x: A): Option[A] = Some(x)

          def ap[A, B](ff: Option[A => B])(fa: Option[A]): Option[B] =
          // we are avoiding flatMap/map to avoid allocating more closures
            if (ff.isDefined && fa.isDefined) Some(ff.get(fa.get))
            else None
        }

      val add: Int => Int => Int =
        x => y => x + y

      val partialResult: Option[Int => Int] =
        Applicative[Option].ap(add.pure[Option])(Option(2))

      val result: Option[Int] =
        Applicative[Option].ap(partialResult)(Option(5))

      result mustBe Option(7)
    }
  }

  "Validation logic" should {
    "be abstract where we have one constraint of our abstract type F - it has to have an instance of ApplicativeError; a special kind of Applicative, which has an additional error type and additional operations" in {
      import cats.data._
      import cats.implicits._
      import cats._

      // Create a custom error type for User validation:
      sealed trait UserValidationError

      case object NameNotValid extends UserValidationError

      case object AgeOutOfRange extends UserValidationError

      case object EmailNotValid extends UserValidationError

      object UserValidator {
        def apply[F[_]: UserValidator]: UserValidator[F] =
          implicitly

        def validate[F[_]: UserValidator, E](name: String, age: Int, email: String): F[User] =
          UserValidator[F].createValidUser(name, age, email)
      }

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

          // After lifting the User.apply function into the Applicative we can just apply the validated values:
          def createValidUser(name: String, age: Int, email: String): F[User] =
            User.curried.pure[F] <*> validateName(name) <*> validateAge(age) <*> validateEmail(email)
            // i.e. (User.apply _).curried.pure[F] <*> validateName(name) <*> validateAge(age) <*> validateEmail(email)
        }

      // Now it's quite easy to create concrete UserValidator instances. We have to provide the target type, the error type and the error constructor function.
      // Here are some examples for Option, Try and Either.

      // Option:
      val userValidatorOptionInterpreter: UserValidator[Option] =
        userValidator[Option, Unit](_ => ())

      {
        implicit val userValidatorInterpreter: UserValidator[Option] =
          userValidatorOptionInterpreter

        println(UserValidator.validate("John", -1, "bad-email"))
      }

      // Try
      val userValidatorTryInterpreter: UserValidator[Try] =
        userValidator[Try, Throwable](err => new Throwable(err.toString))

      {
        implicit val userValidatorInterpreter: UserValidator[Try] =
          userValidatorTryInterpreter

        println(UserValidator.validate("John", -1, "bad-email"))
      }

      // Either
      val userValidatorEitherInterpreter: UserValidator[UserValidationError Either *] =
        userValidator[UserValidationError Either *, UserValidationError](identity)

      {
        implicit val userValidatorInterpreter: UserValidator[UserValidationError Either *] =
          userValidatorEitherInterpreter

        println(UserValidator.validate("John", -1, "bad-email"))
      }

      // ALL THE ABOVE FAIL FAST

      // If we want to collect error messages we can use the type Validated from Cats.
      val userValidatorValidatedInterpreter: UserValidator[Validated[NonEmptyList[UserValidationError], *]] =
        userValidator[Validated[NonEmptyList[UserValidationError], *], NonEmptyList[UserValidationError]](NonEmptyList.one)

      {
        implicit val userValidatorInterpreter: UserValidator[Validated[NonEmptyList[UserValidationError], *]] =
          userValidatorValidatedInterpreter

        println(UserValidator.validate("John", -1, "bad-email"))
      }

      // The only constraint here is that the error type has to have a Semigroup instance. This holds e.g. for List or NonEmptyList.
    }
  }
}