package com.backwards.http4s

import scala.util.control.NonFatal
import cats.{ApplicativeError, MonadError}
import cats.data.{Kleisli, OptionT}
import org.scalatest.matchers.must.Matchers
import org.scalatest.wordspec.AnyWordSpec
import cats.effect.{IO, Sync}
import cats.effect.concurrent.Ref
import cats.syntax.all._
import io.circe.generic.auto._
import io.circe.syntax._
import org.http4s._
import org.http4s.circe._
import org.http4s.circe.CirceEntityDecoder._
import org.http4s.dsl.Http4sDsl

/**
 * @see [[https://typelevel.org/blog/2018/08/25/http4s-error-handling-mtl.html]]
 * @see [[https://typelevel.org/blog/2018/11/28/http4s-error-handling-mtl-2.html]]
 */
class ErrorHandlingSpec extends AnyWordSpec with Matchers {
  "" should {
    "" in {
      import com.olegpy.meow.hierarchy._

      implicit val userRoutes: UserRoutes[IO] = new UserRoutes[IO]

      UserInterpreter.create[IO].flatMap { userAlgebra =>
        val routes = new UserRoutesMTL[IO](userAlgebra)
        IO.unit // pretend this is the rest of your program
      }
    }
  }
}

final case class User(username: String, age: Int)
final case class UserUpdateAge(age: Int)

trait UserAlgebra[F[_]] {
  def find(username: String): F[Option[User]]
  def save(user: User): F[Unit]
  def updateAge(username: String, age: Int): F[Unit]
}

sealed trait UserError extends Exception

final case class UserAlreadyExists(username: String) extends UserError
final case class UserNotFound(username: String) extends UserError
final case class InvalidUserAge(age: Int) extends UserError

object UserInterpreter {
  def create[F[_]: Sync]: F[UserAlgebra[F]] =
    Ref.of[F, Map[String, User]](Map.empty).map { ref =>
      new UserAlgebra[F] {
        private def validateAge(age: Int): F[Unit] =
          if (age <= 0) Sync[F].raiseError(InvalidUserAge(age)) else Sync[F].unit

        override def find(username: String): F[Option[User]] =
          ref.get.map(_.get(username))

        override def save(user: User): F[Unit] =
          validateAge(user.age) *>
            find(user.username).flatMap {
              case Some(_) =>
                Sync[F].raiseError(UserAlreadyExists(user.username))
              case None =>
                ref.update(_.updated(user.username, user))
            }

        override def updateAge(username: String, age: Int): F[Unit] =
          validateAge(age) *>
            find(username).flatMap {
              case Some(user) =>
                ref.update(_.updated(username, user.copy(age = age)))
              case None =>
                Sync[F].raiseError(UserNotFound(username))
            }
      }
    }
}

/**
 * This particular implementation is missing a very important part: error handling.
 * If we use the UserAlgebra’s interpreter previously defined we will clearly miss the three errors defined by the UserError ADT.
 * @param userAlgebra UserAlgebra
 * @tparam F Effect has Sync
 */
class UserRoutesWithoutErrorHandling[F[_]: Sync](userAlgebra: UserAlgebra[F]) extends Http4sDsl[F] {
  val routes: HttpRoutes[F] = HttpRoutes.of[F] {
    case GET -> Root / "users" / username =>
      userAlgebra.find(username).flatMap {
        case Some(user) => Ok(user.asJson)
        case None => NotFound(username.asJson)
      }

    case req @ POST -> Root / "users" =>
      req.as[User].flatMap { user =>
        userAlgebra.save(user) *> Created(user.username.asJson)
      }

    case req @ PUT -> Root / "users" / username =>
      req.as[UserUpdateAge].flatMap { userUpdate =>
        userAlgebra.updateAge(username, userUpdate.age) *> Ok(username)
      }
  }
}

/*
Kleisli is at the core of http4s

Essentially:
HttpRoutes[F] =:= Kleisli[F, Request[F], Option[Response[F]]
*/

object ErrorHandlingWithMonadError extends App {
  import cats.effect.IO

  val boom: IO[String] = IO.raiseError(new Exception("boom"))

  val safe: IO[Throwable Either String] = boom.attempt

  println(safe.unsafeRunSync())

  // Equivalent to:
  import cats.MonadError

  val M = MonadError[IO, Throwable]

  val boom2: IO[String] = M.raiseError(new Exception("boom 2"))

  val safe2: IO[Throwable Either String] = M.attempt(boom2)

  println(safe2.unsafeRunSync())

  // And maybe you want to "map" your business errors, but "keep going":
  val keepGoing = boom.handleErrorWith {
    case NonFatal(t) => IO(println(t.getMessage)) *> IO.pure("Keep going...")
  }

  println(keepGoing.unsafeRunSync())
}

/*
Given business errors:

We could make use of handleErrorWith to transform "business error" into an appropriate "http error response"
Remember, our initial routes above are not handling and doing anything with errors e.g.

case GET -> Root / "users" =>
  retrieveUsers.flatMap(users => Ok(users)).handleErrorWith {
    case UserNotFound(u) => NotFound(s"User not found: ${u.value}")
    case UserAlreadyExists(u) => Conflicts(s"User already exists: ${u.value}")
  }

where

def retrieveUsers: F[List[User]]
*/

/*
We could extract out the error handling part and have it implicitly available:
class HttpErrorHandler[F[_]: Monad] extends Http4sDsl[F] {
  val errorMapping: UserError => F[Response[F]] = {
    case UserNotFound(u) => NotFound(s"User not found: ${u.value}")
    case UserAlreadyExists(u) => Conflicts(s"User already exists: ${u.value}")
  }
}

class UserRoutesWithoutErrorHandling[F[_]: MonadError[*[_], Throwable]: HttpErrorHandler](userAlgebra: UserAlgebra[F]) extends Http4sDsl[F] {
  val routes: HttpRoutes[F] = HttpRoutes.of[F] {
    case GET -> Root / "users" / username =>
      userAlgebra.find(username).flatMap(Ok).handleErrorWith(HttpErrorHandler[F].errorMapping)
*/

/**
 * First attempt at error handling using MonadError which we'll have implicitly because of our constraint Sync[F].
 * As above we start with the simplest approach of using handleErrorWith provided by ApplicativeError.
 * WARNING - We are not handling the UserNotFound error and the compiler didn’t tell us about it!
 * The problem is that our constraint of type Sync from cats-effect has a MonadError instance with its type error fixed as Throwable.
 * So the compiler can’t help us here since this type is too generic.
 * And we can’t add a constraint for MonadError[F, UserError] because we would get an “ambigous implicits” error with two instances of MonadError in scope.
 * @param userAlgebra UserAlgebra
 * @tparam F Effect has Sync
 */
class UserRoutesErrorHandlingFirstAttempt[F[_]: Sync](userAlgebra: UserAlgebra[F]) extends Http4sDsl[F] {
  val routes: HttpRoutes[F] = HttpRoutes.of[F] {
    case GET -> Root / "users" / username =>
      userAlgebra.find(username).flatMap {
        case Some(user) => Ok(user.asJson)
        case None => NotFound(username.asJson)
      }

    case req @ POST -> Root / "users" =>
      req.as[User].flatMap { user =>
        userAlgebra.save(user) *> Created(user.username.asJson)
      }.handleErrorWith {
        case UserAlreadyExists(username) => Conflict(username.asJson)
      }

    case req @ PUT -> Root / "users" / username =>
      req.as[UserUpdateAge].flatMap { userUpdate =>
        userAlgebra.updateAge(username, userUpdate.age) *> Ok(username.asJson)
      }.handleErrorWith {
        case InvalidUserAge(age) => BadRequest(s"Invalid age $age".asJson)
      }
  }
}

/*
Solution - MTL

But first; classy optics:
- Lenses as getters and setters that compose making the accessing of nested data structure’s fields quite easy.
- Prisms as first-class pattern matching that let us access branches of an ADT and that also compose.

UserRoute has a Sync[F] constraint, meaning that we have available a MonadError[F, Throwable] instance,
but we would like to have MonadError[F, UserError] instead to leverage the Scala compiler.

meow-mtl makes heavy use of Shapeless in order to derive Lenses and Prisms and it provides instances for some cats-effect compatible datatypes.

Two of the supported typeclasses are ApplicativeError and MonadError
as long as the error type is a subtype of Throwable to make it compatible with cats-effect e.g.
*/
object MtlExample extends App {
  import cats.MonadError
  import cats.effect.IO
  import com.olegpy.meow.hierarchy._ // All you need is this import!
  import scala.util.Random

  case class CustomError(msg: String) extends Throwable

  /*
  This way OR nicer next way
  def customHandle[F[_], A](f: F[A], fallback: F[A])(implicit ev: MonadError[F, CustomError]): F[A] =
    f.handleErrorWith(_ => fallback)
  */

  def customHandle[F[_]: MonadError[*[_], CustomError], A](f: F[A], fallback: F[A]): F[A] =
    f.handleErrorWith(_ => fallback)

  val io: IO[Int] = IO(Random.nextInt(2)).flatMap {
    case 1 => IO.raiseError(new Exception("boom"))
    case x => IO(x)
  }

  val r: IO[Int] = customHandle(io, IO.pure(123))
  println(r.unsafeRunSync())
}

/*
We can’t have a MonadError[F, UserError] constraint because there’s already a MonadError[F, Throwable] in scope given our Sync[F] constraint.
But we can make this work if we also abstract over the error handling by introducing an algebra of Http handler that can handle our custom errors, where the error type is a subtype of Throwable.
*/

trait Routes[F[_], E <: Throwable] {
  def run(routes: HttpRoutes[F]): HttpRoutes[F]
}

object Routes {
  def apply[F[_]: Routes[*[_], E], E <: Throwable]: Routes[F, E] =
    implicitly[Routes[F, E]]
}

class UserRoutesMTL[F[_]: Sync: Routes[*[_], UserError]](userAlgebra: UserAlgebra[F]) extends Http4sDsl[F] {
  val routes: HttpRoutes[F] = Routes[F, UserError] run HttpRoutes.of[F] {
    case GET -> Root / "users" / username =>
      userAlgebra.find(username).flatMap {
        case Some(user) => Ok(user.asJson)
        case None => NotFound(username.asJson)
      }

    case req @ POST -> Root / "users" =>
      req.as[User].flatMap { user =>
        userAlgebra.save(user) *> Created(user.username.asJson)
      }

    case req @ PUT -> Root / "users" / username =>
      req.as[UserUpdateAge].flatMap { userUpdate =>
        userAlgebra.updateAge(username, userUpdate.age) *> Created(username.asJson)
      }
  }
}

/*
We are basically delegating the error handling (AKA mapping business errors to appropiate http responses) to a specific algebra.
*/

/*
Implement the Routes algebra for our specific error type.
*/
class UserRoutes[F[_]: MonadError[*[_], UserError]] extends Routes[F, UserError] with Http4sDsl[F] {
  private val handler: UserError => F[Response[F]] = {
    case InvalidUserAge(age) => BadRequest(s"Invalid age $age".asJson)
    case UserAlreadyExists(username) => Conflict(username.asJson)
    case UserNotFound(username) => NotFound(username.asJson)
  }

  // Kleisli[F, Request[F], Option[Response[F]]
  def run(routes: HttpRoutes[F]): HttpRoutes[F] = {
    Kleisli { req =>
      OptionT(routes.run(req).value.handleErrorWith(e => handler(e).map(Option.apply)))
    }
  }
}

/*
Bio functors such as IO[E, A] would be nice to use.
We'll simply add an "error channel"
*/

trait ErrorChannel[F[_], E <: Throwable] {
  def raise[A](e: E): F[A]
}

object ErrorChannel {
  def apply[F[_]: ErrorChannel[*[_], E], E <: Throwable]: ErrorChannel[F, E] =
    implicitly[ErrorChannel[F, E]]

  implicit def instance[F[_]: ApplicativeError[*[_], E], E <: Throwable]: ErrorChannel[F, E] =
    new ErrorChannel[F, E] {
      override def raise[A](e: E): F[A] =
        ApplicativeError[F, E].raiseError(e)
    }

  object syntax {
    implicit class ErrorChannelOps[F[_]: ErrorChannel[*[_], E], E <: Throwable](e: E) {
      def raise[A]: F[A] =
        ErrorChannel[F, E].raise[A](e)
    }
  }
}

/*
Now include the error channel with our algebra
*/

abstract class UserAlgebraHasErrorChannel[F[_]: ErrorChannel[*[_], E], E <: Throwable] {
  def find(username: String): F[Option[User]]
  def save(user: User): F[Unit]
  def updateAge(username: String, age: Int): F[Unit]
}

object UserInterpreterHasErrorChannel {
  def create[F[_]: Sync: ErrorChannel[*[_], UserError]]: F[UserAlgebraHasErrorChannel[F, UserError]] =
    Ref.of[F, Map[String, User]](Map.empty).map { state =>
      new UserAlgebraHasErrorChannel[F, UserError] {
        private def validateAge(age: Int): F[Unit] =
          if (age <= 0) ErrorChannel[F, UserError].raise(InvalidUserAge(age)) else ().pure[F]

        override def find(username: String): F[Option[User]] =
          state.get.map(_.get(username))

        override def save(user: User): F[Unit] =
          validateAge(user.age) *>
            find(user.username).flatMap {
              case Some(_) =>
                // ErrorChannel[F, UserError].raise(new Exception("asd")) Does not compile, expect UserError
                // Sync[F].raiseError(new Exception("boom")) Should be considered an unrecoverable failure
                ErrorChannel[F, UserError].raise(UserAlreadyExists(user.username))

              case None =>
                state.update(_.updated(user.username, user))
            }

        override def updateAge(username: String, age: Int): F[Unit] =
          validateAge(age) *>
            find(username).flatMap {
              case Some(user) =>
                state.update(_.updated(username, user.copy(age = age)))

              case None =>
                ErrorChannel[F, UserError].raise(UserNotFound(username))
            }
      }
    }
}

/*
Notice that we could still call Sync[F].raiseError(new Exception("boom")) and it will still compile.
However, if we choose to use ErrorChannel to signal business errors we will have the compiler on our side
and it’ll warn us when we try to raise an error that is not part of the ADT we have declared.
So signaling error in a different way should just be considered unrecoverable.
*/