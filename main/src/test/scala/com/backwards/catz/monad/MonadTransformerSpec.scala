package com.backwards.catz.monad

import cats.data.EitherT
import cats.effect.IO
import org.scalatest.matchers.must.Matchers
import org.scalatest.wordspec.AnyWordSpec

/**
  * In simple terms a monad transformer is a piece of code that understands how to handle a specific monad e.g. Option.
  * It knows how to transform this monad into a different shape.
  *
  * Regarding monad transformers, two of the usual suspects are OptionT and EitherT.
  *
  * Let's take a look at OptionT's type parameters:
  * OptionT[F[_], A].
  *
  * We need to specify the effect wrapping the option (the other monad e.g. IO in the below examples) and the inner type (User or Order in our case).
  *
  * Once we have an OptionT transformer we can perform the usual "map", "flatMap" and "flatten" operations that we're used to.
  *
  * And as mentioned there is an EitherT:
  * EitherT[F[_], A, B]
  */
class MonadTransformerSpec extends AnyWordSpec with Matchers {
  case class User()

  case class Order()

  sealed trait ServiceError

  case class UserNotFound(userId: Int) extends ServiceError

  case class OrderNotFound(user: User) extends ServiceError

  "Code with nested Option" should {
    def fetchUser(id: Int): IO[Option[User]] = IO(Option(User()))

    def fetchOrder(user: User): IO[Option[Order]] = IO(Option(Order()))

    "not compile without monad transformers" in {
      """
      val userAndOrder: IO[Option[(User, Order)]] = for {
        user <- fetchUser(1)
        order <- fetchOrder(user) // Doesn't compile
      } yield (user, order)
      """ mustNot compile
    }

    "work with nasty workaround of again not using monad transformers" in {
      val userAndOrder: IO[Option[(User, Order)]] = for {
        maybeUser <- fetchUser(1)
        maybeOrder <- maybeUser.map(fetchOrder).getOrElse(IO(None))
      } yield for {
        user <- maybeUser
        order <- maybeOrder
      } yield (user, order)

      userAndOrder.unsafeRunSync() mustBe Option(User() -> Order())
    }

    "work with monad transformer, OptionT in this case" in {
      import cats.data.OptionT

      val userAndOrder: OptionT[IO, (User, Order)] = for {
        user <- OptionT(fetchUser(1))
        order <- OptionT(fetchOrder(user))
      } yield (user, order)

      // Note the use of .value call because we're actually flatMapping OptionT not IO[Option[A]]
      userAndOrder.value.unsafeRunSync() mustBe Option(User() -> Order())
    }
  }

  "Code with nested Either" should {
    def fetchUser(id: Int): IO[UserNotFound Either User] = IO(Right(User()))

    def fetchOrder(user: User): IO[OrderNotFound Either Order] = IO(Right(Order()))

    "not compile" in {
      """
      val userAndOrder = for {
        user <- EitherT(fetchUser(1))
        order <- EitherT(fetchOrder(user))
      } yield (user, order)
      """ mustNot compile

      /*
      Error: type mismatch;
      found   : User => EitherT[IO, OrderNotFound, (User, Order)]
      required: User => EitherT[IO, Product with Serializable with ServiceError, (User, Order)]
                user <- EitherT(fetchUser(1))
      */

      /*
      The problem is that we're trying to sequence Eithers with two different left types, UserNotFound and OrderNotFound.
      We know that they are both ServiceErrors but in this case Cats is invariant of types.
      As far as Cats is concerned UserNotFound and OrderNotFound are unrelated.
      */
    }

    "compile by specifying a top level Left" in {
      def fetchUser(id: Int): IO[ServiceError Either User] = IO(Right(User()))

      def fetchOrder(user: User): IO[ServiceError Either Order] = IO(Right(Order()))

      val userAndOrder: EitherT[IO, ServiceError, (User, Order)] = for {
        user <- EitherT(fetchUser(1))
        order <- EitherT(fetchOrder(user))
      } yield (user, order)

      userAndOrder.value.unsafeRunSync() mustBe Right(User() -> Order())
    }

    "compile by forcing our specific eithers to be more generic" in {
      val userAndOrder: EitherT[IO, ServiceError, (User, Order)] = for {
        user <- EitherT(fetchUser(1): IO[ServiceError Either User])
        order <- EitherT(fetchOrder(user): IO[ServiceError Either Order])
      } yield (user, order)

      userAndOrder.value.unsafeRunSync() mustBe Right(User() -> Order())
    }
  }
}