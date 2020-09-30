package com.backwards.fp.free

import scala.concurrent.Future
import cats.~>
import org.scalatest.matchers.must.Matchers
import org.scalatest.wordspec.AnyWordSpec

/**
 * [[https://softwaremill.com/free-monads/]]
 *
 * A Monad has a
 *  - pure: A => M[A] and
 *  - flatMap: M[A] => (A => M[B]) => M[B]
 *
 * Monads can be used to describe how to compute a value in a context using a sequence of steps.
 */
class FreeMonad3Spec extends AnyWordSpec with Matchers {
  final case class Tickets(count: Int)

  "Free" should {
    "motivate" in {
      /*
      We will describe the external interactions as data using a family of case classes.
      Each external interaction results in data of some type (specified as a type parameter). E.g.
      */

      // Common type for all external interactions yielding data of type A
      sealed trait External[A]

      final case class InvokeTicketingService(count: Int) extends External[Tickets]

      /*
      Formally, we want to define a data structure

      Program[External, A]
      parametrised by the type of the base instructions and the result of the whole program.
      */

      /*
      Having a description of a program, and an interpretation for our base instruction set, we'd like to extend this interpretation to cover the programs we have built.
      More specifically, given a function

      interpretBase[A]: External[A] => M[A]
      where M is any monad,
      we'd like to extend this to a function

      interpretProgram[A]: Program[External, A] => M[A]

      It would be nice if there was only one way to create such a (well-behaving) extension so that we don't have to make any additional choices.

      So,
      Program[External, A] = Free[External, A]
      that is it's the free monad over External.
      */
    }

    "present" in {
      /*
      To generalise, in the following our above External[_] is S[_].

      Our programs will be abstract syntax trees expressed as nested case class instances!
      The base trait here is:

      Free[S[_], A]
      which is a program returning a value of type A with basic instructions of type S[_]:
      */
      trait Free[S[_], A]

      case class Pure[S[_], A](value: A) extends Free[S, A]

      case class FlatMap[S[_], A, B](p: Free[S, A], f: A => Free[S, B]) extends Free[S, B]

      case class Suspend[S[_], A](s: S[A]) extends Free[S, A]
    }

    "why use?" in {
      // Without Free we might describe a service ala OO style:
      trait TicketingServiceV1 {
        def invoke(count: Int): Tickets
      }

      /*
      This looks similar to the
      case class InvokeTicketingService(count: Int) extends External[Tickets]
      with the main difference being that we now have a method instead of a class.
      However, note that the method signature constrains us in how the method can be implemented:
      it needs to be synchronous, as we return a strict Tickets value, not e.g. a Future[Tickets].

      So we could instead do:
      */
      trait TicketingServiceV2 {
        def invoke(count: Int): Future[Tickets]
      }

      /*
      Doesn't really solve the problem.
      The details on what kind of side effects we allow and how they are interpreted already leaked into the service interface
      (we only allow futures).
      */
    }

    "go free" in {
      import scala.concurrent.ExecutionContext.Implicits.global
      import cats.free.Free

      case class UserTicketsRequest(ticketCount: Int)
      case class Tickets(count: Int)

      sealed trait External[A]

      case class InvokeTicketingService(count: Int) extends External[Tickets]

      object serviceInvoker {
        def run(path: String): Future[Tickets] =
          Future(Tickets(11))
      }

      def purchaseTickets(input: UserTicketsRequest): Free[External, Option[Tickets]] =
        if (input.ticketCount > 0) {
          // creates a "Suspend" node
          Free.liftF(InvokeTicketingService(input.ticketCount)).map(Some(_))
        } else {
          Free.pure(None)
        }

      def bonusTickets(purchased: Option[Tickets]): Free[External, Option[Tickets]] =
        if (purchased.exists(_.count > 10)) {
          Free.liftF(InvokeTicketingService(1)).map(Some(_))
        } else {
          Free.pure(None)
        }

      def formatResponse(purchased: Option[Tickets], bonus: Option[Tickets]): String =
        s"Purchased tickets: $purchased, bonus: $bonus"

      val input = UserTicketsRequest(11)

      val logic: Free[External, String] = for {
        purchased <- purchaseTickets(input)
        bonus <- bonusTickets(purchased)
      } yield formatResponse(purchased, bonus)

      val externalToServiceInvoker = new (External ~> Future) {
        override def apply[A](e: External[A]): Future[A] = e match {
          case InvokeTicketingService(c) => serviceInvoker.run(s"/tkts?count=$c")
        }
      }

      val result = logic.foldMap(externalToServiceInvoker)
      result.foreach(println)

      /*
      The logic value is just a description of a program - nothing has been executed yet.
      We can now run it by providing an interpretation of External e.g. to Future:

      val externalToServiceInvoker = new (External ~> Future) {
        override def apply[A](e: External[A]): Future[A] = e match {
          case InvokeTicketingService(c) => serviceInvoker.run(s"/tkts?count=$c")
        }
      }

      val result: Future[String] = logic.foldMap(externalToServiceInvoker)
      // show the result to the user

      Or in tests, we can provide an alternative interpretation:

      val testingInterpeter = new (External ~> Id) {
        override def apply[A](e: External[A]): Id[A] = e match {
          case InvokeTicketingService(c) => Tickets(10)
        }
      }

      val result: String = logic.foldMap(testingInterpeter)
      // assert that the result is correct
      */
    }
  }
}