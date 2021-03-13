package com.backwards.fp

import org.scalatest.matchers.must.Matchers
import org.scalatest.wordspec.AnyWordSpec
import cats.data.{EitherK, EitherT}
import cats.free.Free
import cats.{InjectK, Monad, ~>}
import cats.implicits._
import scala.collection.mutable
import scala.concurrent.{Await, Future}
import scala.concurrent.duration._
import scala.concurrent.ExecutionContext.Implicits.global

/**
 * [[https://medium.com/@agaro1121/free-monad-vs-tagless-final-623f92313eac Free Monad vs Tagless Final]]
 */
class FreeMonadVsTaglessSpec extends AnyWordSpec with Matchers {
  "The initial API we will be converting" should {
    trait Database[T] {
      def create(t: T): Future[Boolean]

      def read(id: Long): Future[DatabaseError Either T]

      def delete(id: Long): Future[DatabaseError Either Unit]
    }

    "create your DSLs - Tagless Final" in {
      TaglessApp
    }

    "create your DSLs - Free" in {
      FreeApp
    }
  }

  "Now mixing in multiple algebra - adding console algebra to database" should {
    /** Vanilla API */
    trait ConsoleVanillaAlgebra {
      def putLine[T](t: T): Unit
    }

    /** Tagless Final */
    trait ConsoleAlgebra[F[_]] {
      def putLine[T](t: T): F[Unit]
    }

    /** Free */
    sealed trait ConsoleFreeAlgebraT[T]

    case class PutLine[T](t: T) extends ConsoleFreeAlgebraT[Unit]

    "still create DSLs - Tagless Final" in {
      TaglessWithSecondAlgebraApp
    }

    "still create DSLs - Free" in {
      FreeWithSecondAlgebraApp
    }
  }
}

final case class User(id: Long, name: String, age: Int)

abstract class DatabaseError extends Throwable

case object ErrorFindingUser extends DatabaseError

case object ErrorUpdatingUser extends DatabaseError

final case class ErrorDeletingUser(msg: String) extends DatabaseError

object TaglessApp extends App {
  trait DatabaseAlgebra[F[_], T] {
    def create(t: T): F[Boolean]

    def read(id: Long): F[DatabaseError Either T]

    def delete(id: Long): F[DatabaseError Either Unit]
  }

  /*
  So we just took the future out and moved it up to the trait declaration in the form of F[_].
  Now instead of everything returning Future[_], it returns F[_] and F can be whatever Monad you want.
  For example: Future, IO, Id, Task, etc...
  */

  /*
  As it stands, this does nothing useful - We need to create some interpreters to interpret our algebra into actions
  */

  /*
  We'll create interpreters to execute our actions using Futures (though we could use some other Monad).
  */

  object DatabaseAlgebra {
    val FutureInterpreter: DatabaseAlgebra[Future, User] =
      new DatabaseAlgebra[Future, User] {
        val users: mutable.Map[Long, User] = mutable.Map.empty

        override def create(user: User): Future[Boolean] = {
          val inserted: Option[User] = users.put(user.id, user)
          Future.successful(inserted.isEmpty || inserted.isDefined)
        }

        override def read(id: Long): Future[DatabaseError Either User] =
          Future.successful(users.get(id).toRight(ErrorFindingUser))

        override def delete(id: Long): Future[DatabaseError Either Unit] = {
          import cats.syntax.either._ // For the .asLeft[] - This is also another smart constructor to help the compiler along.

          val deleted: Option[User] = users.remove(id)

          Future.successful(
            deleted.fold(ErrorDeletingUser(s"User with Id($id) was not there").asLeft[Unit])(_ => Right(()))
          )
        }
      }
  }

  /*
  Now that our code can actually do stuff, let’s write some repos to wrap our low-level DB code:
  */

  class UserRepo[F[_]: Monad](DB: DatabaseAlgebra[F, User]) {
    def getUser(id: Long): F[DatabaseError Either User] = DB.read(id)

    def addUser(user: User): F[Boolean] = DB.create(user)

    def updateUser(user: User): F[DatabaseError Either Boolean] =
      (for {
        userFromDB <- EitherT(getUser(user.id))
        successfullyAdded <- EitherT.liftF[F, DatabaseError, Boolean](addUser(user))
      } yield successfullyAdded).value
  }

  val repo: UserRepo[Future] =
    new UserRepo(DatabaseAlgebra.FutureInterpreter)

  println(
    Await.result(
      for {
        _ <- repo.addUser(User(1, "Bob", 31))
        dbErrorOrSuccessfullyUpdated <- repo.updateUser(User(1, "Bobby", 31))
      } yield dbErrorOrSuccessfullyUpdated,
      1 second
    )
  )
}

object TaglessWithSecondAlgebraApp extends App {
  import TaglessApp.DatabaseAlgebra

  trait ConsoleAlgebra[F[_]] {
    def putLine[T](t: T): F[Unit]
  }

  object ConsoleAlgebra {
    val FutureInterpreter: ConsoleAlgebra[Future] =
      new ConsoleAlgebra[Future] {
        override def putLine[T](t: T): Future[Unit] =
          Future successful println(t)
      }
  }

  // And the hard part - combining the algebras
  class UserRepo[F[_]: Monad](
    DB: DatabaseAlgebra[F, User],
    C: ConsoleAlgebra[F] // Only difference is here
  ) {
    def getUser(id: Long): F[DatabaseError Either User] = DB.read(id)

    def addUser(user: User): F[Boolean] = DB.create(user)

    def updateUser(user: User): F[DatabaseError Either Boolean] =
      (for {
        userFromDB <- EitherT(getUser(user.id))
        _ <- EitherT.liftF(C.putLine(s"We found user($userFromDB)!!")) // This is new
        successfullyAdded <- EitherT.liftF[F, DatabaseError, Boolean](addUser(user))
      } yield successfullyAdded).value
  }

  val repo: UserRepo[Future] =
    new UserRepo(DatabaseAlgebra.FutureInterpreter, ConsoleAlgebra.FutureInterpreter) // <- Console interpreter simply added in

  println(
    Await.result(
      for {
        _ <- repo.addUser(User(1, "Bob", 31))
        dbErrorOrSuccessfullyUpdated <- repo.updateUser(User(1, "Bobby", 31))
      } yield dbErrorOrSuccessfullyUpdated,
      1 second
    )
  )
}

object FreeApp extends App {
  /** This is your ADT - specifically, a GADT */
  sealed trait DBFreeAlgebraT[T]

  final case class Create(user: User) extends DBFreeAlgebraT[Boolean]

  final case class Read(id: Long) extends DBFreeAlgebraT[DatabaseError Either User]

  final case class Delete(id: Long) extends DBFreeAlgebraT[DatabaseError Either Unit]
  /********************************************************/

  object DBFreeAlgebraT {
    type DBFreeAlgebra[T] = Free[DBFreeAlgebraT, T]

    // Smart constructors
    def create(user: User): DBFreeAlgebra[Boolean] =
      Free.liftF[DBFreeAlgebraT, Boolean](Create(user))

    def read(id: Long): DBFreeAlgebra[DatabaseError Either User] =
      Free.liftF[DBFreeAlgebraT, DatabaseError Either User](Read(id))

    def delete(id: Long): DBFreeAlgebra[DatabaseError Either Unit] =
      Free.liftF[DBFreeAlgebraT, DatabaseError Either Unit](Delete(id))

    /*
    Free is encoding your algebra as ADTs so we need all the case classes.
    We also need the smart constructors because our case classes are too specific.
    Smart constructors create instances of our case classes but the return type is the more generalized super type: DBFreeAlgebraT[T].
    */

    /*
    As it stands, this does nothing useful - We need to create some interpreters to interpret our algebra into actions
    */

    /*
    We'll create interpreters to execute our actions using Futures (though we could use some other Monad).
    */

    val FutureInterpreter: DBFreeAlgebraT ~> Future =
      new (DBFreeAlgebraT ~> Future) {
        /**
         * The above is equivalent to the signature below
         * `val FutureInterpreter = new FunctionK[DBFreeAlgebraT, Future]`
         *
         * It's like a function on values from A => B
         *
         * The only difference this works on "Kinds" hence `FunctionK`
         * This basically says you are creating a function from F[A] => F[B]
         *
         * We're converting our free structure (F[A]) to a known Monad (F[B])
         * In this case, we're converting DBFreeAlgebra[T] => Future[T]
         */
        val users: mutable.Map[Long, User] = mutable.Map.empty

        override def apply[A](fa: DBFreeAlgebraT[A]): Future[A] =
          fa match {
            case Create(user) => // F[A]
              val inserted: Option[User] = users.put(user.id, user)

              Future.successful(inserted.isEmpty || inserted.isDefined).asInstanceOf[Future[A]] // F[B]

            case Read(id) => // F[A]
              Future.successful(users.get(id).toRight(ErrorFindingUser)).asInstanceOf[Future[A]] // F[B]

            case Delete(id) => // F[A]
              import cats.syntax.either._

              val deleted: Option[User] = users.remove(id)

              Future.successful(
                deleted.fold(ErrorDeletingUser(s"User with Id($id) was not there").asLeft[Unit])(_ => Right(()))
              ).asInstanceOf[Future[A]] // F[B]
          }
      }

    /*
    Now that our code can actually do stuff, let’s write some repos to wrap our low-level DB code:
    */

    class FreeUserRepo {
      val DB: DBFreeAlgebraT.type = DBFreeAlgebraT

      def getUser(id: Long): DBFreeAlgebra[DatabaseError Either User] = DB.read(id) // This is a program

      def addUser(user: User): DBFreeAlgebra[Boolean] = DB.create(user) // This is a program

      // This is a program
      def updateUser(user: User): DBFreeAlgebra[DatabaseError Either Boolean] =
        (for {
          userFromDB <- EitherT(getUser(user.id))
          successfullyAdded <- EitherT.liftF[DBFreeAlgebra, DatabaseError, Boolean](addUser(user))
        } yield successfullyAdded).value
    }

    /*
    Notice that the tagless final version accepts its interpreter as an argument while the Free version does not.
    The one big difference is the tagless final actually runs your code and returns your expected result.
    The Free version simply builds a recursive structure - Something like Suspend(Suspend(Pure(...)).
    The structure gets broken down and traversed through a little later in the main method via the interpreter.
    */
  }

  import com.backwards.fp.FreeApp.DBFreeAlgebraT.{FreeUserRepo, FutureInterpreter}

  val repo: FreeUserRepo =
    new FreeUserRepo

  println(
    Await.result(
      (for {
        _ <- repo.addUser(User(2, "Bob", 31))
        dbErrorOrSuccessfullyUpdated <- repo.updateUser(User(2, "Bobby", 31))
      } yield dbErrorOrSuccessfullyUpdated).foldMap(FutureInterpreter), // Notice the foldMap(..) here
      1 second
    )
  )

  /*
  In the Free version, we pass it in when we fold over our structure and map it to a Future.

  In the Free version, our code STILL doesn’t actually do anything until you call .foldMap.
  The calls repo.addUser(User(2, "Bob", 31)) and repo.updateUser(User(2, "Bobby", 31)) simply build a recursive data structure.
  This can can grow very large and that's fine because the cats implementation is stack safe.
  The .foldMap combinator folds over our structure every step at a time and converts that step to our known Monad using the FutureInterpreter.
  */
}

object FreeWithSecondAlgebraApp extends App {
  sealed trait ConsoleFreeAlgebraT[T]

  case class PutLine[T](t: T) extends ConsoleFreeAlgebraT[Unit]

  object ConsoleFreeAlgebraT {
    type ConsoleAlgebra[T] = Free[ConsoleFreeAlgebraT, T]

    def putLine[T](t: T): ConsoleFreeAlgebraT[Unit] = PutLine(t)

    val FutureInterpreter: ConsoleFreeAlgebraT ~> Future =
      new (ConsoleFreeAlgebraT ~> Future) {
        override def apply[A](fa: ConsoleFreeAlgebraT[A]): Future[A] =
          fa match {
            case PutLine(t) =>
              Future.successful(println(t)).asInstanceOf[Future[A]]
          }
      }
  }

  /*
  Multiple algebras is a tad more involved with regards to Free.
  Since our algebras are multiple ADTs, we need to tell our code that any given line of code in our program could be from our DB algebra OR from the console algebra.
  Essentially, it's either one of these. This can be 2 or more algebras.
  But before we can even get to that point we need to add a little boilerplate.
  We have to make our distinct algebras have a common umbrella that they will fall under: Free[_, _]
  That's what we have smart constructors for:
  */

  import FreeApp.DBFreeAlgebraT
  import FreeApp.{Create, Delete, Read}

  /** Step 1 - Wrap your smart constructors in a class with an implicit `InjectK` */
  class DBFreeAlgebraTI[F[_]](implicit I: InjectK[DBFreeAlgebraT, F]) {
    /** Step 2 - instead of calling `.liftF` you call `.inject` */
    def create(user: User): Free[F, Boolean] = // <- All algebras are `Free` now
      Free.inject[DBFreeAlgebraT, F](Create(user))

    def read(id: Long): Free[F, DatabaseError Either User] = // <- All algebras are `Free` now
      Free.inject[DBFreeAlgebraT, F](Read(id))

    def delete(id: Long): Free[F, DatabaseError Either Unit] = // <- All algebras are `Free` now
      Free.inject[DBFreeAlgebraT, F](Delete(id))
  }

  /** Step 3 - create an implicit instance of your new class */
  implicit def dBFreeAlgebraTI[F[_]](implicit I: InjectK[DBFreeAlgebraT, F]): DBFreeAlgebraTI[F] =
    new DBFreeAlgebraTI[F]

  /** Rinse and repeat for our console algebra */
  class ConsoleFreeAlgebraTI[F[_]](implicit I: InjectK[ConsoleFreeAlgebraT, F]) {
    def putLine[T](t: T): Free[F, Unit] = Free.inject[ConsoleFreeAlgebraT, F](PutLine(t)) // <- All algebras are `Free` now
  }

  implicit def consoleFreeAlgebraTI[F[_]](implicit I: InjectK[ConsoleFreeAlgebraT, F]): ConsoleFreeAlgebraTI[F] =
    new ConsoleFreeAlgebraTI[F]

  // Now that the boilerplate is out of the way, we can tell our code that we have many algebras:
  object Combined {
    type DbAndConsoleAlgebra[T] = EitherK[DBFreeAlgebraT, ConsoleFreeAlgebraT, T]

    val FutureInterpreter: DbAndConsoleAlgebra ~> Future =
      DBFreeAlgebraT.FutureInterpreter or ConsoleFreeAlgebraT.FutureInterpreter
  }


  class FreeUserRepo(
    implicit DB: DBFreeAlgebraTI[Combined.DbAndConsoleAlgebra], C: ConsoleFreeAlgebraTI[Combined.DbAndConsoleAlgebra]
  ) {
    def getUser(id: Long): Free[Combined.DbAndConsoleAlgebra, DatabaseError Either User] = DB.read(id)

    def addUser(user: User): Free[Combined.DbAndConsoleAlgebra, Boolean] = DB.create(user)

    /**
     * EitherT.liftF has the following signature:
     * def liftF[F[_], A, B](fb: F[B])
     *
     * We need this type alias because of the fact it only accepts an `F[_]`
     * and we're passing in `Free[DbAndConsoleAlgebra, A]`
     * The type alias fixes 1 parameter to fit the mold of `F[_]`
     */
    type DbAndConsoleAlgebraContainer[A] = Free[Combined.DbAndConsoleAlgebra, A]

    def updateUser(user: User): Free[Combined.DbAndConsoleAlgebra, DatabaseError Either Boolean] =
      (for {
        userFromDB <- EitherT(getUser(user.id))
        _ <- EitherT.liftF(C.putLine(s"We found user($userFromDB)!!"))
        successfullyAdded <- EitherT.liftF[DbAndConsoleAlgebraContainer, DatabaseError, Boolean](addUser(user))
      } yield successfullyAdded).value
  }

  val repo: FreeUserRepo = new FreeUserRepo

  println(
    Await.result(
      (for {
        _ <- repo.addUser(User(1, "Bob", 31))
        dbErrorOrSuccessfullyUpdated <- repo.updateUser(User(1, "Bobby", 31))
      } yield dbErrorOrSuccessfullyUpdated).foldMap(Combined.FutureInterpreter), // <- New shiny interpreter
      1 second
    )
  )
}