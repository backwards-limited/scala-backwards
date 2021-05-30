package com.backwards.catz.monad

import scala.concurrent.duration._
import scala.concurrent.{Await, Future}
import scala.language.postfixOps
import scala.util.Try
import cats.MonadError
import cats.data.EitherT
import cats.syntax.either._
import org.scalatest.concurrent.ScalaFutures
import org.scalatest.matchers.must.Matchers
import org.scalatest.wordspec.AnyWordSpec

/**
  * Scala's Either type allows us to deal with two paths of execution (Left or Right).
  * Futures do the same (Success or Failure).
  * Stacking Future and Either results in three execution paths (Success-Left, Success-Right, Failure) and it's easy to forget about the third scenario.
  * Cats' MonadError allows us to reduce (Success-Left, Success-Right, Failure) to (Left or Right) by transforming a failed future into a Success-Left.
  *
  * A monad that also allows you to raise and or handle an error value. This type class allows one to abstract over error-handling monads.
  */
class MonadErrorSpec extends AnyWordSpec with Matchers with ScalaFutures {
  import scala.concurrent.ExecutionContext.Implicits.global
  import cats.instances.future._

  case class User()

  case class Order()

  sealed trait ServiceError

  case class UserNotFound(userId: Int) extends ServiceError

  case class OrderNotFound(user: User) extends ServiceError

  case class OtherError(message: String) extends ServiceError

  def getUserId: UserNotFound Either Int = Right(1)

  def fetchUser(id: Int): Future[UserNotFound Either User] = Future.successful(Right(User()))

  def fetchOrder(user: User): Future[OrderNotFound Either Order] = Future.successful(Right(Order()))

  "Without MonadError" should {
    "step 1 - see that we can forget about the edge case of a failed future" in {
      val eitherT = for {
        userId <- EitherT.fromEither[Future](getUserId: Either[ServiceError, Int])
        user <- EitherT(fetchUser(userId): Future[ServiceError Either User])
        order <- EitherT(fetchOrder(user): Future[ServiceError Either Order])
      } yield order

      whenReady(eitherT.value) { order =>
        order mustBe Order().asRight
      }

      // Apart from the crappy futures and assertions, so far so good, at the end of the flow we will either have a ServiceError or an Order.
      // Well actually that's not quite true because the Futures themselves can fail. It's easy to forget about this edge case
    }

    "step 2 - use recover/recoverWith and get more mess" in {
      val eitherT = for {
        userId <- EitherT.fromEither[Future](getUserId: Either[ServiceError, Int])
        user <- EitherT((fetchUser(userId): Future[ServiceError Either User]).recover { case t => OtherError(t.getMessage).asLeft[User] })
        order <- EitherT((fetchOrder(user): Future[ServiceError Either Order]).recover { case t => Left(OtherError(t.getMessage)) })
      } yield order

      whenReady(eitherT.value) { order =>
        order mustBe Order().asRight
      }
    }
  }

  "MonadError" should {
    val eitherT = for {
      userId <- EitherT.fromEither[Future](getUserId: Either[ServiceError, Int])
      user <- EitherT(fetchUser(userId): Future[ServiceError Either User])
      order <- EitherT(fetchOrder(user): Future[ServiceError Either Order])
    } yield order

    "step 1" in {
      /**
        * MonadError.apply is:
        * def apply[M[_], E] so it expects an M[_] (the underlying Monad) and an E (the error/failure type - in our case Throwable).
        *
        * MonadError expects a Monad in the shape M[_] but our EitherT is actually M[F[_], A, B] so we need to fix two of the three types to give us M[_].
        * We could use an explicit type for this i.e.
        *
        * type MyEitherT[A] = EitherT[Future, ServiceError, A]
        * MonadError[MyEitherT, Throwable].recoverWith(eitherT) { ... }
        *
        * But we can also use the kind compiler plugin to create an anonymous type for us. That's what's happening
        * in the EitherT[Future, ServiceError, *] call below
        */
      val recoveredEitherT = MonadError[EitherT[Future, ServiceError, *], Throwable].recoverWith(eitherT) {
        case t => EitherT.leftT[Future, Order](OtherError(s"future failed - ${t.getMessage}"): ServiceError)
      }
    }

    "step 2" in {
      implicit class RecoveringEitherT[F[_], A, B](underlying: EitherT[F, A, B])(implicit me: MonadError[F, Throwable]) {
        def recoverF(op: Throwable => A): EitherT[F, A, B] = MonadError[EitherT[F, A, *], Throwable].recoverWith(underlying) {
          case t => EitherT.fromEither[F](op(t).asLeft[B])
        }
      }

      /*
      1. Firstly I'm using an implicit class to pimp EitherT, adding a recoverF method

      2. The F[_] type parameter says our F should be a type constructor (wrapper type) e.g. Future/Monix Task/Option etc.
         A represents the Left type and B represents the Right type.

      3. The implicit MonadError[F, Throwable] is the interesting parameter.
         It tells the compiler we need a MonadError instance for our F and Throwable. Cats includes such an implementation for Future

      4. We pass an op parameter which just says given a Throwable, generate our Left type (A)

      5. Finally we use MonadError's recoverWith to transform Throwable to A using the provided op
      */

      // Using the implicit class and recoverF
      val recoveredEitherT = eitherT.recoverF(t => OtherError(s"future failed - ${t.getMessage}"): ServiceError)

      val errorOrUserAndOrder = Await.result(recoveredEitherT.value, 1 second)

      errorOrUserAndOrder.foreach(order => println(s"success: $order"))
      errorOrUserAndOrder.left.foreach(error => println(s"error: $error"))
    }
  }

  "MonadError API" should {
    import cats.effect.IO
    import cats.implicits._
    import cats.effect.unsafe.implicits.global

    "" in {
      def blah[F[_]](s: String)(implicit MonadError: MonadError[F, Throwable]): F[Int] = {
        for {
          x <- Try(s.toInt).fold(t => MonadError.raiseError[Int](new Exception("Whoops")), int => int.pure[F])
        } yield x
      }

      blah[IO]("42").unsafeRunSync() mustBe 42
      an [Exception] must be thrownBy blah[IO]("xx").unsafeRunSync()
    }
  }
}