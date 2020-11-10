package com.backwards.fp

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.{Await, Future}
import scala.concurrent.duration._
import cats.data.NonEmptyList
import org.scalatest.matchers.must.Matchers
import org.scalatest.wordspec.{AnyWordSpec, AsyncWordSpec}

/**
 * [[https://www.matfournier.com/2020-01-10-getting-funcey-part1-types/ Series of 5 FP articles]]
 *
 * [[https://www.matfournier.com/2020-01-11-getting-funcey-part2-effects/ Part 2]]
 *
 */
class Functional2Spec extends AnyWordSpec with Matchers {
  def await[A]: Future[A] => A =
    future => Await.result(future, 5 seconds)

  "Functor" should {
    "be described" in {
      // e.g.
      trait Functor[F[_]] {
        def map[A, B](x: F[A])(f: A => B): F[B]

        // This we can derive from the first, this sort of derivation is common,
        // we can lift any function from A -> B to one that is `F[A] -> F[B]`
        def lift[A, B](f: A => B): F[A] => F[B] =
          fa => map(fa)(f)
      }

      /*
      The following all have the same shape:

      map :: (a -> b) -> Maybe    a -> Maybe    b
      map :: (a -> b) -> List     a -> List     b
      map :: (a -> b) -> Either e a -> Either e b
      map :: (a -> b) -> F        a -> F        b
      */
    }
  }

  "Functors" should {
    "compose" in {
      import cats.implicits._

      // Say you have:
      val xs: List[Option[Future[Int]]] = List(Some(5.pure[Future]), None, Some(9.pure[Future]))

      // And we wish to change the nested Int (if indeed it exists), so we have to:
      xs.map(_.map(_.map(_ + 1)))
      // WTF

      // Instead, compose functors for List and then Option and finally Future:
      import cats.Functor

      // Through Functor#compose
      val result: List[Option[Future[Int]]] =
        Functor[List].compose[Option].compose[Future].map(xs)(_ + 1)

      val List(Some(r1), r2, Some(r3)) = result

      await(r1) mustBe 6
      r2 mustBe None
      await(r3) mustBe 10

      // If effects F and G have Functors, then F[G[_]] or G[F[_]] is also a functor.
      // These properties are important and tend to be described as laws when people talk about type classes.
    }
  }

  "Applicative" should {
    "be described" in {
      /*
      map     ::   (a ->   b) -> f a -> f b      .. FUNCTOR

      <*>     :: f (a ->   b) -> f a -> f b      .. APPLICATIVE

      flatMap ::   (a -> f b) -> f a -> f b      .. MONAD
      (>>=)

      We are applying some function through some context/effect/box/structure except the function is also stuck inside some context.
      */
      trait Functor[F[_]] {
        def map[A, B](x: F[A])(f: A => B): F[B]

        def lift[A, B](f: A => B): F[A] => F[B] =
          fa => map(fa)(f)
      }

      trait Applicative[F[_]] extends Functor[F] {
        def product[A, B](fa: F[A], fb: F[B]): F[(A, B)]

        def pure[A](a: A): F[A]
      }

      /*
      - Given a Task[KafkaAuditLog] and a Task[Response] that have nothing to do with each other, I can run them both and give you a Task[(KafkaAuditLog, Response)]
      - Intuition: product, composing multiple independent effects
      - Intuition: concurrency
      */

      /*
      Suppose we want to aggregate data from 2 remote services and serve responses as fast as we can.
      If we use Monad, we have to hit the first service and (if successful) hit the second service. This is not what I want:
      */
      import monix.eval.Task

      final case class User()
      final case class Data()
      final case class Payload(user: User, data: Data)

      def loadUser: Task[User] = Task delay User()

      def loadData: Task[Data] = Task delay Data()

      // Sequentially
      for {
        user <- loadUser
        data <- loadData
      } yield Payload(user, data)

      // But applicative to the rescue. We can use mapN to compute the independent effects:
      import cats.implicits.catsSyntaxTuple2Semigroupal
      import monix.execution.Scheduler.Implicits.global

      val payload: Task[Payload] = (loadUser, loadData) mapN Payload
      payload.runSyncUnsafe() mustBe Payload(User(), Data())

      /*
      What exactly Applicative does depends on the effect.
      For our Task effect, it will run loadUser and loadData concurrently on two different threads.
      If List was the effect, you would get all combinations of the values in each list.
      */
    }
  }

  "Traverse" should {
    "be described" in {
      trait Functor[F[_]] {
        def map[A, B](x: F[A])(f: A => B): F[B]

        def lift[A, B](f: A => B): F[A] => F[B] =
          fa => map(fa)(f)
      }

      trait Applicative[F[_]] extends Functor[F] {
        def product[A, B](fa: F[A], fb: F[B]): F[(A, B)]

        def pure[A](a: A): F[A]
      }

      trait Traverse[F[_]] {
        def traverse[G[_]: Applicative, A, B](fa: F[A])(f: A => G[B]): G[F[B]]
      }

      /*
      We may have nested types, e.g. List[Future[A]]
      aka we may have F[G[A]]

      We want to flip these inside out, to get G[F[A]]
      We use sequence, which depends on Applicative for this this is the secret sauce.
      */
    }
  }
}