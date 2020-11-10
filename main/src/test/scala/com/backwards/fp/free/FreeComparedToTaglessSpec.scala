package com.backwards.fp.free

import java.util.UUID
import scala.concurrent.Future
import monocle.macros.syntax.lens.toGenApplyLensOps
import org.scalatest.matchers.must.Matchers
import org.scalatest.wordspec.AnyWordSpec

/**
 * [[https://softwaremill.com/free-tagless-compared-how-not-to-commit-to-monad-too-early/]]
 *
 * <pre>
 * Free monad	                                                                              | Final tagless
 * ----------                                                                               | -------------
 * Program is data	                                                                        | Program is an expression
 * Programs are built from constructors (ADT)	                                              | Programs are built from functions
 * Plain values	                                                                            | Expressions parametrized by a type constructor (`F[_]`)
 * Values represent abstract syntax	                                                        | Expressions are denontation of the program in the target monad
 * Interpretation is deffered	                                                              | Interpretation happens when the expression is created
 * Stack-safe	                                                                              | Stack-safety depends on the target monad
 * Each operation is reified as a value	                                                    | Can have less overhead, without creating intermediate objects
 * Easy pattern matching for inspection and optimization	                                  | Pattern matching possible, but harder
 * Combining languages using `Coproduct`s and implicit `Inject` intances, some boilerplate	| Combining languages using multiple algebras, less boilerplate
 * </pre>
 */
class FreeComparedToTaglessSpec extends AnyWordSpec with Matchers {
  final case class User(id: UUID, email: String, loyaltyPoints: Int)

  "Without free or tagless" should {
    "show example" in {
      import scala.concurrent.ExecutionContext.Implicits.global

      sealed trait UserRepository {
        def findUser(id: UUID): Future[Option[User]]
        def updateUser(u: User): Future[Unit]
      }

      class LoyaltyPoints(ur: UserRepository) {
        def addPoints(userId: UUID, pointsToAdd: Int): Future[String Either Unit] =
          ur.findUser(userId).flatMap {
            case None =>
              Future.successful(Left("User not found"))

            case Some(user) =>
              Future.successful(user.lens(_.loyaltyPoints).modify(_ + pointsToAdd))
                .flatMap(ur.updateUser)
                .map(Right.apply)
          }

        // Other methods ...
      }

      /*
      Do we really use any of Futures features anywhere? Is there any reason why this code uses a Future and not, let's say, Task? No!
      The only things we need are: map, flatMap and unit (which is called Future.successful here). In other words, any monad would do.
      */
    }
  }

  "Free" should {
    "show example" in {
      /*
      We first need to define a set of basic instructions, which are represented as data types (they form an ADT):
      */
      import cats.free.Free

      sealed trait UserRepositoryAlg[T]

      case class FindUser(id: UUID) extends UserRepositoryAlg[Option[User]]
      case class UpdateUser(u: User) extends UserRepositoryAlg[Unit]

      type UserRepository[T] = Free[UserRepositoryAlg, T]

      def findUser(id: UUID): UserRepository[Option[User]] =
        Free liftF FindUser(id)

      def updateUser(u: User): UserRepository[Unit] =
        Free liftF UpdateUser(u)

      /*
      The type T specifies the result of the operation.
      Free[UserRepositoryAlg, T] enriches our basic instruction set with the possibility to return pure values and sequence operations;
      it also provides a monad instance so that we can e.g. use for-comprehensions to combine the basic instructions.
      */

      /*
      We also define helper methods findUser and updateUser which lift "bare" instructions into the free monad context.
      */

      /*
      The description of a program which adds points for a user can be now be expressed as follows:
      */
      def addPoints(userId: UUID, pointsToAdd: Int): UserRepository[String Either Unit] =
        findUser(userId).flatMap {
          case None =>
            Free.pure(Left("User not found"))

          case Some(user) =>
            Free.pure(user.lens(_.loyaltyPoints).modify(_ + pointsToAdd))
              .flatMap(updateUser)
              .map(Right.apply)
        }
      /*
      Not that different from the original method e.g. Future.successful -> Free.pure

      The crucial difference is, however, that we return a data structure, a value,
      which uses abstract instructions, without specifying in any way how to interpret those instructions.
      */

      /*
      How about actually running the code? To do that, we need to provide an interpreter (which is separate from problem description):
      */
      import scala.concurrent.ExecutionContext.Implicits.global
      import cats.~>
      import cats.implicits._

      val futureInterpreter = new (UserRepositoryAlg ~> Future) {
        override def apply[A](fa: UserRepositoryAlg[A]): Future[A] = fa match {
          case FindUser(id) =>
            // Go and talk to a database
            Future.successful(None)

          case UpdateUser(u) =>
            // As above
            Future.successful(())
        }
      }

      val result: Future[String Either Unit] =
        addPoints(UUID.randomUUID(), 10).foldMap(futureInterpreter)

      /*
      We need to provide an interpretation for every instruction, resulting in the target monad - here Future.
      We can then interpret the whole program using the foldMap method, which gives us the result (String Either Unit),
      wrapped in our target.
      */
    }
  }

  "Tagless" should {
    "show example" in {
      // With final tagless, we define our basic instruction set (the "algebra") as a trait, parametrized by the resulting container:

      import scala.concurrent.ExecutionContext.Implicits.global
      import cats.Monad
      import cats.implicits._

      trait UserRepositoryAlg[F[_]] {
        def findUser(id: UUID): F[Option[User]]
        def updateUser(u: User): F[Unit]
      }

      class LoyaltyPoints[F[_]: Monad](ur: UserRepositoryAlg[F]) {
        def addPoints(userId: UUID, pointsToAdd: Int): F[String Either Unit] =
          ur.findUser(userId).flatMap {
            case None =>
              Monad[F].pure(Left("User not found"))

            case Some(user) =>
              Monad[F].pure(user.lens(_.loyaltyPoints).modify(_ + pointsToAdd))
                .flatMap(ur.updateUser)
                .map(Right.apply)
          }
      }

      // And now to run the code:

      trait FutureInterpreter extends UserRepositoryAlg[Future] {
        override def findUser(id: UUID): Future[Option[User]] =
          Future successful None // Go and talk to a database

        override def updateUser(u: User): Future[Unit] =
          Future successful () // As above
      }

      val result: Future[String Either Unit] =
        new LoyaltyPoints(new FutureInterpreter {}).addPoints(UUID.randomUUID(), 10)

      /*
      Unlike with the free monad, here stack safety depends on the stack safety of the target monad (in case of Future we're fine).

      Both here and with the free monad approach, we can quite easily define interpreters dedicated for testing, which would interpret side-effects
      e.g. into the identity (Id) monad, making the test code easier to read and reason about.
      */
    }
  }

  "Without free or tagless" should {
    "show more in-depth example" in {
      /*
      Moving on from a single domain-specific language to multiple sets of such operations, of multiple languages.
      Let's email after updating the loyal points.
      */
      import scala.concurrent.ExecutionContext.Implicits.global

      case class User(id: UUID, email: String, loyaltyPoints: Int)

      trait EmailService {
        def sendEmail(email: String, subject: String, body: String): Future[Unit]
      }

      trait UserRepository {
        def findUser(id: UUID): Future[Option[User]]
        def updateUser(u: User): Future[Unit]
      }

      class LoyaltyPoints(ur: UserRepository, es: EmailService) {
        def addPoints(userId: UUID, pointsToAdd: Int): Future[String Either Unit] =
          ur.findUser(userId).flatMap {
            case None =>
              Future successful Left("User not found")

            case Some(user) =>
              for {
                user <- Future successful user.lens(_.loyaltyPoints).modify(_ + pointsToAdd)
                _ <- ur updateUser user
                _ <- es.sendEmail(user.email, "Points added!", s"You now have ${user.loyaltyPoints}")
              } yield Right(())
          }
      }
    }
  }

  "Free" should {
    "show more in-depth example" in {
      // As before, the basic instructions are represented as data (ADTs). We now have two case class families:
      sealed trait UserRepositoryAlg[T]
      case class FindUser(id: UUID) extends UserRepositoryAlg[Option[User]]
      case class UpdateUser(u: User) extends UserRepositoryAlg[Unit]

      sealed trait EmailAlg[T]
      case class SendEmail(email: String, subject: String, body: String) extends EmailAlg[Unit]

      /*
      If we want to build programs which use both languages, we need to combine these instructions sets into one.
      We can do this with a Coproduct, which is Scala's Either, but for containers (types of the form F[_]).
      Though note that EitherK is now used in Cats instead of Coproduct (maybe because Shapeless has its own CoProduct):
      */
      import scala.concurrent.ExecutionContext.Implicits.global
      import cats.data.EitherK // import cats.data.Coproduct
      import cats.InjectK
      import cats.free.Free
      import cats.~>

      type UserAndEmailAlg[T] = EitherK[UserRepositoryAlg, EmailAlg, T]
      /*
      The eitherk/coproduct is yet another ADT which wraps one of two ADTs:
      either a user instruction on the left,
      or an email instruction on the right.
      Our (mini) program will now be Free[UserAndEmailAlg, T]
      */

      /*
      To wrap a basic instruction, e.g. FindUser, into the target Free[UserAndEmailAlg, T] type,
      we now not only need to wrap it in Free.liftF, but also in Coproduct.leftc.
      It would be quite tedious to do this by hand, especially if we'd combine a larger number of languages.
      That's where the Inject/InjectK typeclass comes into play; it automates the embedding of a combined language into the target type.
      Note that InjectK is for type constructor types i.e. higher kinded types (a bit like EitherK for higher kinded types).
      */
      class Users[F[_]](implicit i: InjectK[UserRepositoryAlg, F]) {
        def findUser(id: UUID): Free[F, Option[User]] = Free.inject(FindUser(id))
        def updateUser(u: User): Free[F, Unit] = Free.inject(UpdateUser(u))
      }

      object Users {
        implicit def users[F[_]](implicit i: InjectK[UserRepositoryAlg, F]): Users[F] =
          new Users
      }

      class Emails[F[_]](implicit i: InjectK[EmailAlg, F]) {
        def sendEmail(email: String, subject: String, body: String): Free[F, Unit] =
          Free.inject(SendEmail(email, subject, body))
      }

      object Emails {
        implicit def emails[F[_]](implicit i: InjectK[EmailAlg, F]): Emails[F] =
          new Emails
      }

      // Finally, we can implement the addPoints method:

      def addPoints(userId: UUID, pointsToAdd: Int)(
        implicit us: Users[UserAndEmailAlg],
        es: Emails[UserAndEmailAlg]
      ): Free[UserAndEmailAlg, String Either Unit] = {

        us.findUser(userId).flatMap {
          case None =>
            Free.pure(Left("User not found"))

          case Some(user) =>
            for {
              user <- Free.pure(user.lens(_.loyaltyPoints).modify(_ + pointsToAdd))
              _ <- us.updateUser(user)
              _ <- es.sendEmail(user.email, "Points added!", s"You now have ${user.loyaltyPoints}")
            } yield Right(())
        }
      }

      // We can specify the interpreters for the instruction sets independently, and then combine them:
      val futureUserInterpreter = new (UserRepositoryAlg ~> Future) {
        override def apply[A](fa: UserRepositoryAlg[A]): Future[A] = fa match {
          case FindUser(id) =>
            // Go and talk to a database
            Future successful None

          case UpdateUser(u) =>
            // As above
            Future successful ()
        }
      }

      val futureEmailInterpreter = new (EmailAlg ~> Future) {
        override def apply[A](fa: EmailAlg[A]): Future[A] = fa match {
          case SendEmail(email, subject, body) =>
            // Use smtp
            Future successful ()
        }
      }

      val futureUserOrEmailInterpreter = futureUserInterpreter or futureEmailInterpreter

      val result: Future[String Either Unit] =
        addPoints(UUID.randomUUID(), 10).foldMap(futureUserOrEmailInterpreter)
    }
  }

  "Tagless" should {
    "show more in-depth example" in {
      import scala.concurrent.ExecutionContext.Implicits.global
      import cats.Monad
      import cats.implicits._

      trait UserRepositoryAlg[F[_]] {
        def findUser(id: UUID): F[Option[User]]
        def updateUser(u: User): F[Unit]
      }

      trait EmailAlg[F[_]] {
        def sendEmail(email: String, subject: String, body: String): F[Unit]
      }

      class LoyaltyPoints[F[_]: Monad](us: UserRepositoryAlg[F], es: EmailAlg[F]) {
        def addPoints(userId: UUID, pointsToAdd: Int): F[String Either Unit] =
          us.findUser(userId).flatMap {
            case None =>
              Monad[F].pure(Left("User not found"))

            case Some(user) =>
              for {
                user <- Monad[F].pure(user.lens(_.loyaltyPoints).modify(_ + pointsToAdd))
                _ <- us.updateUser(user)
                _ <- es.sendEmail(user.email, "Points added!", s"You now have ${user.loyaltyPoints}")
              } yield Right(())
          }
      }

      // For interpretation, we now need to create two traits, each implementing the corresponding instruction set:

      trait FutureUserInterpreter extends UserRepositoryAlg[Future] {
        override def findUser(id: UUID): Future[Option[User]] =
          Future successful None  // Go and talk to a database

        override def updateUser(u: User): Future[Unit] =
          Future successful () // As above
      }

      trait FutureEmailInterpreter extends EmailAlg[Future] {
        override def sendEmail(email: String, subject: String, body: String): Future[Unit] =
          Future successful () // Use smtp
      }

      val result: Future[Either[String, Unit]] =
        new LoyaltyPoints(new FutureUserInterpreter {}, new FutureEmailInterpreter {})
          .addPoints(UUID.randomUUID(), 10)
    }
  }

  "One instruction in the high-level language can be translated to multiple instructions in the lower-level languages" should {
    "be done by specifying upfront the target effect (in this case a Future)" in {
      import scala.concurrent.ExecutionContext.Implicits.global

      case class User(id: UUID, email: String, loyaltyPoints: Int) {
        def serialize: String = s"${id.toString}, $loyaltyPoints, $email"
      }

      object User {
        def parse(s: String): User = {
          val parts: Array[String] = s.split(",")
          User(UUID.fromString(parts(0)), parts(2), parts(1).toInt)
        }
      }

      trait KVStore {
        def get(k: String): Future[Option[String]]
        def put(k: String, v: String): Future[Unit]
      }

      trait UserRepository {
        def findUser(id: UUID): Future[Option[User]]
        def updateUser(u: User): Future[Unit]
      }

      class UserRepositoryUsingKVStore(kvStore: KVStore) extends UserRepository {
        override def findUser(id: UUID): Future[Option[User]] =
          kvStore.get(id.toString).map(serialized => serialized.map(User.parse))

        override def updateUser(u: User): Future[Unit] =
          for {
            serialized <- Future successful u.serialize
            _ <- kvStore.put(u.id.toString, serialized)
            _ <- kvStore.put(u.email, serialized) // Let's say we also maintain a by-email index
          } yield ()
      }

      /*
      What's new is an implementation of UserRepository which users a KVStore.
      How can we express this using free monads or tagless final?
      */
    }

    "free" in {
      import scala.concurrent.ExecutionContext.Implicits.global
      import cats.free.Free
      import cats.~>

      case class User(id: UUID, email: String, loyaltyPoints: Int) {
        def serialize: String = s"${id.toString}, $loyaltyPoints, $email"
      }

      object User {
        def parse(s: String): User = {
          val parts: Array[String] = s.split(",")
          User(UUID.fromString(parts(0)), parts(2), parts(1).toInt)
        }
      }

      sealed trait KVAlg[T]

      case class Get(k: String) extends KVAlg[Option[String]]
      case class Put(k: String, v: String) extends KVAlg[Unit]

      type KV[T] = Free[KVAlg, T]

      def get(k: String): KV[Option[String]] = Free.liftF(Get(k))
      def put(k: String, v: String): KV[Unit] = Free.liftF(Put(k, v))

      sealed trait UserRepositoryAlg[T]

      case class FindUser(id: UUID) extends UserRepositoryAlg[Option[User]]
      case class UpdateUser(u: User) extends UserRepositoryAlg[Unit]

      // First, we interpret UserRepositoryAlg in terms of KV

      val userToKvInterpreter = new (UserRepositoryAlg ~> KV) {
        override def apply[A](fa: UserRepositoryAlg[A]): KV[A] = fa match {
          case FindUser(id) =>
            get(id.toString).map(_.map(User.parse))

          case UpdateUser(u) =>
            for {
              serialized <- Free.pure(u.serialize)
              _ <- put(u.id.toString, serialized)
              _ <- put(u.email, serialized) // We also maintain a by-email index
            } yield ()
        }
      }

      // Now we need a second stage interpreter, which interprets KVAlg instructions in terms of our target monad, in our example that's Future:

      val kvToFutureInterpreter = new (KVAlg ~> Future) {
        override def apply[A](fa: KVAlg[A]): Future[A] = fa match {
          case Get(k) => /* Go and talk to a database */ Future successful None
          case Put(k, v) => /* As above */ Future successful ()
        }
      }

      // Note that we no longer need an interpreter from UserRepositoryAlg to Future - the intermediate language does that for us:

      type UserRepository[T] = Free[UserRepositoryAlg, T]

      def findUser(id: UUID): UserRepository[Option[User]] = Free.liftF(FindUser(id))
      def updateUser(u: User): UserRepository[Unit] = Free.liftF(UpdateUser(u))

      def addPoints(userId: UUID, pointsToAdd: Int): UserRepository[String Either Unit] =
        findUser(userId).flatMap {
          case None =>
            Free.pure(Left("User not found"))

          case Some(user) =>
            Free.pure(user.lens(_.loyaltyPoints).modify(_ + pointsToAdd))
              .flatMap(updateUser)
              .map(Right.apply)
        }

      val result: Future[String Either Unit] =
        addPoints(UUID.randomUUID(), 10)
          .foldMap(userToKvInterpreter)
          .foldMap(kvToFutureInterpreter)
    }

    "tagless" in {
      import scala.concurrent.ExecutionContext.Implicits.global
      import cats.Monad
      import cats.implicits._

      case class User(id: UUID, email: String, loyaltyPoints: Int) {
        def serialize: String = s"${id.toString}, $loyaltyPoints, $email"
      }

      object User {
        def parse(s: String): User = {
          val parts: Array[String] = s.split(",")
          User(UUID.fromString(parts(0)), parts(2), parts(1).toInt)
        }
      }

      // Again, we first define the lower-level language, and it's interpretation to Future:

      trait KVAlg[F[_]] {
        def get(k: String): F[Option[String]]
        def put(k: String, v: String): F[Unit]
      }

      trait KvToFutureInterpreter extends KVAlg[Future] {
        override def get(k: String): Future[Option[String]] =
          Future successful None // Go and talk to a database

        override def put(k: String, v: String): Future[Unit] =
          Future successful () // As above
      }

      // We have to create an interpreter for UserRepositoryAlg in terms of KVAlg:

      trait UserRepositoryAlg[F[_]] {
        def findUser(id: UUID): F[Option[User]]
        def updateUser(u: User): F[Unit]
      }

      class UserThroughKvInterpreter[F[_]: Monad](kv: KVAlg[F]) extends UserRepositoryAlg[F] {
        override def findUser(id: UUID): F[Option[User]] =
          kv.get(id.toString).map(_.map(User.parse))

        override def updateUser(u: User): F[Unit] =
          for {
            serialized <- Monad[F].pure(u.serialize)
            _ <- kv.put(u.id.toString, serialized)
            _ <- kv.put(u.email, serialized) // We also maintain a by-email index
          } yield ()
      }

      class LoyaltyPoints[F[_]: Monad](ur: UserRepositoryAlg[F]) {
        def addPoints(userId: UUID, pointsToAdd: Int): F[String Either Unit] = {
          ur.findUser(userId).flatMap {
            case None =>
              Monad[F].pure(Left("User not found"))

            case Some(user) =>
              Monad[F].pure(user.lens(_.loyaltyPoints).modify(_ + pointsToAdd))
                .flatMap(ur.updateUser)
                .map(Right.apply)
          }
        }
      }

      val result: Future[String Either Unit] =
        new LoyaltyPoints(new UserThroughKvInterpreter(new KvToFutureInterpreter {}))
          .addPoints(UUID.randomUUID(), 10)
    }
  }
}