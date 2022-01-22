package tech.backwards.fp.effects

import scala.concurrent.{Future, TimeoutException}
import cats.data.Validated.{Invalid, Valid}
import cats.data.{EitherT, NonEmptyList, Reader, Writer}
import cats.implicits._
import cats.{Applicative, Id, Monad}
import monix.eval.Task
import org.scalatest.concurrent.ScalaFutures
import org.scalatest.matchers.must.Matchers
import org.scalatest.wordspec.AnyWordSpec

/**
 * [[https://www.matfournier.com/2019-07-25-effects/]]
 */
class EffectSpec extends AnyWordSpec with Matchers with ScalaFutures {
  final case class User(name: String, id: Long, age: Double)

  "Monad" should {
    def doThing[F[_]: Monad](a: F[String], b: F[Long], c: F[Double]): F[User] =
      for {
        aa <- a
        bb <- b
        cc <- c
      } yield User(aa, bb, cc)

    "Option" in {
      // Good
      doThing("mat".some, 17828382L.some, 1.3.some) mustBe User("mat", 17828382L, 1.3).some

      // Bad
      doThing("mat".some, none[Long], 1.3.some) mustBe none[User]
    }

    "Either" in {
      type ThrowableOr[A] = Throwable Either A

      // Good
      doThing("mat".asRight: ThrowableOr[String], 17828382L.asRight, 1.3.asRight) mustBe User("mat", 17828382L, 1.3).asRight

      // Bad
      val Left(t) = doThing(new java.lang.NoSuchFieldError("whoops").asLeft[String], 17828382L.asRight, 1.3.asRight)
      t mustBe a [NoSuchFieldError]
    }

    "Future" in {
      import scala.concurrent.ExecutionContext.Implicits.global

      // Good
      whenReady(doThing("mat".pure[Future], 17828382L.pure[Future], 1.3.pure[Future])) {
        _ mustBe User("mat", 17828382L, 1.3)
      }

      // Bad
      whenReady(doThing("mat".pure[Future], Future.failed[Long](new TimeoutException("whoops")), 1.3.pure[Future]).failed) {
        _ mustBe a [TimeoutException]
      }
    }

    // TODO - IO

    "Task" in {
      import monix.execution.Scheduler.Implicits.global

      // Good
      // TODO - Fails to compile and upgrade to Cats Effect 3
      // doThing(Task("mat"), Task(17828382L), Task(1.3)).runSyncUnsafe() mustBe User("mat", 17828382L, 1.3)

      // Bad
      // TODO - Fails to compile and upgrade to Cats Effect 3
      /*val Left(t) = doThing(Task("mat"), Task.raiseError[Long](new TimeoutException("whoops")), Task(1.3)).attempt.runSyncUnsafe()
      t mustBe a [TimeoutException]*/
    }

    "EitherT - for a Task[Throwable Either A]" in {
      import monix.execution.Scheduler.Implicits.global

      type TaskOfThrowableOr[A] = EitherT[Task, Throwable, A]

      // Good
      // TODO - Fails to compile and upgrade to Cats Effect 3
      // doThing(EitherT(Task("mat".asRight)), EitherT(Task(17828382L.asRight)), EitherT(Task(1.3.asRight))).value.runSyncUnsafe() mustBe User("mat", 17828382L, 1.3).asRight
    }

    "Reader" in {
      final case class Injected(version: String, idShift: Long, ageShift: Double)

      type Config[A] = Reader[Injected, A]

      // Good
      val r: Reader[Injected, User] =
        doThing(Reader[Injected, String](_.version + ":mat"), Reader[Injected, Long](_.idShift + 17828382L), Reader[Injected, Double](_.ageShift + 1.3))

      val user: Id[User] = r.run(Injected("3.2", 200000L, 37.2))
      user mustBe User("3.2:mat", 18028382L, 38.5)
    }

    "Writer" in {
      final case class Computation(notes: String, money: Int)

      type Trace[A] = Writer[List[Computation], A]

      val user: Trace[String] =
        "mat".pure[Trace] <* List(Computation("fetched user", 100)).tell

      val id: Trace[Long] =
        17827382L.pure[Trace] <* List(Computation("fetched id", 1000)).tell

      val age: Trace[Double] =
        1.3.pure[Trace] <* List(Computation("fetched age", 10000)).tell

      val t: Trace[User] = doThing(user, id, age)
      val (computations: List[Computation], tracedUser: User) = t.run

      computations mustBe List(Computation("fetched user", 100), Computation("fetched id", 1000), Computation("fetched age", 10000))
      tracedUser mustBe User("mat", 17827382L, 1.3)
    }
  }

  "Applicative" should {
    def doThing[F[_]: Applicative](a: F[String], b: F[Long], c: F[Double]): F[User] = {
      (a, b, c).mapN(User.apply)
    }

    "Validation" in {
      // Good
      doThing("mat".validNel, 17828382L.validNel, 1.3.validNel) mustBe Valid(User("mat", 17828382L, 1.3))

      // Bad
      doThing("Username invalid".invalidNel[String], 17828382L.validNel, "Age invalid".invalidNel[Double]) mustBe Invalid(NonEmptyList.of("Username invalid", "Age invalid"))
    }
  }
}