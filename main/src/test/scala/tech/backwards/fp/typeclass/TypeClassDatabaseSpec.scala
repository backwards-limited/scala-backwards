package tech.backwards.fp.typeclass

import scala.util.{Success, Try}
import cats.data.EitherT
import cats.{Id, Monad}
import org.scalatest.matchers.must.Matchers
import org.scalatest.wordspec.AnyWordSpec

class TypeClassDatabaseSpec extends AnyWordSpec with Matchers {
  final case class User(id: Int, name: String)

  "Without type class" should {
    "prove to be inconvenient" in {
      trait Database {
        def load(id: Int): User
        def save(user: User): Unit
      }

      def updateUser(userId: Int, newName: String)(db: Database): User = {
        val user = db.load(userId)
        val updated = user.copy(name = newName)

        db.save(updated)
        updated
      }

      /*
      The API is fairly simple, I can provide an in-memory implementation of this for testing very easily.
      But what about the implementation used in production? Will it block while interacting with some external database?
      We can’t let that happen, so we need to change the signatures to return, say Futures.

      But when the Database returns Futures such as Future[User], then our tests become awkward because of non-determinism.
      */
    }
  }

  "With type class" should {
    "improve things" in {
      /*
      What if we found a way to abstract over the particular type constructor that this trait works with?
      So instead of hard coding Future, we can use whatever else type constructor we fancy. We can make it a typeclass.
      */

      trait Database[F[_]] {
        def load(id: Int): F[User]
        def save(user: User): F[Unit]
      }

      // Testing remains easy as we can use Id
      object FakeDatabase extends Database[Id] {
        def load(id: Int): User = User(id, "testname")
        def save(user: User): Unit = ()
      }

      /*
      How do we implement the updateUser method?
      If only there were a way of chaining operations, just like we do with semicolons if no obscure type constructors are in the way.
      Do this, and then do this, and then return this. Sounds rather imperative, doesn’t it?
      */
      trait ImperativeCombinator[F[_]] {
        def doAndThen[A, B](fa: F[A])(f: A => F[B]): F[B]
        def returns[A](a: A): F[A]
      }

      def updateUser[F[_]](userId: Int, newName: String)(db: Database[F], imp: ImperativeCombinator[F]): F[User] = {
        imp.doAndThen(db.load(userId)) { user =>
          val updated = user.copy(name = newName)

          imp.doAndThen(db.save(updated)) { _ =>
            imp.returns(updated)
          }
        }
      }

      // If this ImperativeCombinator seems rather familiar it is because it is known as the Monad.
    }
  }

  "With type class (our own) and from a library such as Cats" should {
    "improve things even more" in {
      trait Database[F[_]] {
        def load(id: Int): F[User]
        def save(user: User): F[Unit]
      }

      object Database {
        // We can provide some instances in the companion object, if we like
        implicit val dbId: Database[Id] = new Database[Id] {
          def load(id: Int): User = User(id, "some name")
          def save(user: User): Unit = ()
        }

        def apply[F[_]: Database]: Database[F] = implicitly[Database[F]]

        object syntax {
          def load[F[_]: Database](id: Int): F[User] =
            Database[F].load(id)

          def save[F[_]: Database](user: User): F[Unit] =
            apply[F].save(user)
        }
      }

      import Database.syntax._
      import cats.implicits._

      def updateUser[F[_] : Database : Monad](userId: Int, newName: String): F[User] =
        for {
          user <- load(userId)
          updated = user.copy(name = newName)
          _ <- save(user.copy(name = newName))
        } yield updated
    }
  }

  "Steps towards using MonadError with our type class" should {
    /*
    We can implement an instance for any type constructor, and the problem is we know nothing here about errors.
    The different type constructors have different ways to approach errors, or none at all.
    Think about how different it is to deal with a failed Future from dealing with a Left for example. What can we do about it?
    */

    "attempt to handle without MonadError by using Either" in {
      trait Database[F[_]] {
        def load(id: Int): F[Throwable Either User]
        def save(user: User): F[Throwable Either Unit]
      }

      object Database {
        def apply[F[_]: Database]: Database[F] = implicitly[Database[F]]

        object syntax {
          def load[F[_]: Database](id: Int): F[Throwable Either User] =
            Database[F].load(id)

          def save[F[_]: Database](user: User): F[Throwable Either Unit] =
            apply[F].save(user)
        }
      }

      import Database.syntax._
      import cats.syntax.flatMap._
      import cats.syntax.functor._

      def updateUser[F[_] : Database : Monad](userId: Int, newName: String): F[Either[Throwable, User]] =
        load(userId) flatMap {
          case Right(user) =>
            val updated = user.copy(name = newName)

            save(updated) map {
              case Right(_) => Right(updated)
              case Left(e) => Left(e)
            }

          case left => Monad[F].pure(left)
        }
    }

    "attempt to handle without MonadError by using EitherT" in {
      trait Database[F[_]] {
        def load(id: Int): EitherT[F, Throwable, User]
        def save(user: User): EitherT[F, Throwable, Unit]
      }

      object Database {
        def apply[F[_]: Database]: Database[F] = implicitly[Database[F]]

        object syntax {
          def load[F[_]: Database](id: Int): EitherT[F, Throwable, User] =
            Database[F].load(id)

          def save[F[_]: Database](user: User): EitherT[F, Throwable, Unit] =
            apply[F].save(user)
        }
      }

      import Database.syntax._

      def updateUser[F[_] : Database : Monad](userId: Int, newName: String): EitherT[F, Throwable, User] =
        for {
          user <- load(userId)
          updated = user.copy(name = newName)
          _ <- save(updated)
        } yield updated
    }

    /*
    The above 2 approaches are not nice - Enter ApplicativeError:
    There is a typeclass, called ApplicativeError. This extends Applicative with capabilities for dealing with errors.
    The two main functions are raiseError and handleErrorWith.
    The first is like pure, but for errors, it lifts an error value to the context of F.
    The latter is responsible for handling errors, potentially recovering from it.
    */
  }

  "Using MonadError with our type class" should {
    trait Database[F[_]] {
      def load(id: Int): F[User]
      def save(user: User): F[Unit]
    }

    object Database {
      def apply[F[_]: Database]: Database[F] = implicitly[Database[F]]

      object syntax {
        def load[F[_]: Database](id: Int): F[User] =
          Database[F].load(id)

        def save[F[_]: Database](user: User): F[Unit] =
          apply[F].save(user)
      }
    }

    import Database.syntax._
    import cats.MonadError
    import cats.syntax.applicativeError._
    import cats.syntax.flatMap._
    import cats.syntax.functor._

    def updateWithLog[F[_]: Database: MonadError[*[_], Throwable]](userId: Int, newName: String): F[User] =
      updateUser(userId, newName).map { updated =>
        println("success")
        updated
      }.recoverWith { case error =>
        println(error)
        MonadError[F, Throwable].raiseError(error)
      }

    def updateUser[F[_]: Database: MonadError[*[_], Throwable]](userId: Int, newName: String): F[User] = for {
      user <- if (userId == 99) MonadError[F, Throwable].raiseError(new Exception("whoops")) else load(userId)
      updated = user.copy(name = newName)
      _ <- save(updated)
    } yield updated

    "work" in {
      import cats.implicits._

      implicit val tryDatabase: Database[Try] = new Database[Try] {
        def load(id: Int): Try[User] = Success(User(id, "bob"))

        def save(user: User): Try[Unit] = Success(())
      }

      println(updateUser[Try](1, "bob"))


      implicit val eitherDatabase: Database[Throwable Either *] = new Database[Throwable Either *] {
        def load(id: Int): Throwable Either User = Right(User(id, "bob"))

        def save(user: User): Throwable Either Unit = Right(())
      }

      println(updateUser[Throwable Either *](1, "bob"))

      /*
      What happens to Id? Id is gone.
      We can no longer use it in our synchronous implementation for testing,
      we either have to swap that one out for something binary, a data type that can handle two cases,
      say, an Either or a Try, or something lazy such as Eval.
      */

      // And adding some more implicit magic:
      implicit class LogOps[F[_]: MonadError[*[_], Throwable], A](fa: F[A]) {
        def logFailure(msg: Throwable => String): F[A] = fa recoverWith {
          case e: Throwable =>
            println(msg(e))
            fa
        }

        def logInfo(msg: A => String): F[A] = fa flatMap { value =>
          println(msg(value))
          fa
        }
      }

      println(
        updateUser[Try](99, "John")
          .logFailure(e => s"Aw, snap! Failed to update user, because of $e")
          .logInfo(u => s"Successfully updated user: $u")
      )
    }
  }
}