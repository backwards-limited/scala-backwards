package com.backwards.typelevel

import cats.data.EitherT
import org.scalatest.matchers.must.Matchers
import org.scalatest.wordspec.AnyWordSpec

/**
 * [[https://ivan-kurchenko.medium.com/error-composition-with-shapeless-coproducts-b649848339bc Error composition with Shapeless coproducts]]
 */
class ErrorCompositionWithCoProductsSpec extends AnyWordSpec with Matchers {
  /**
   * Naive approach of using EitherT (or indeed Either) shows that composition breaks down.
   */
  "Naive" should {
    import TicketsService._

    final case class User(id: String)

    final case class Ticket(id: String, status: String, lastUpdatedUser: User, lastUpdateTimestamp: Long)

    trait TicketsService[F[_]] {
      final type Result[E, R] = EitherT[F, E, R]

      def getTicket(id: String, currentUser: User): Result[GetError, Ticket]

      def updateTicket(update: Ticket, currentUser: User): Result[UpdateError, Ticket]
    }

    object TicketsService {
      sealed trait GetError

      final case object TicketGetNotExists extends GetError

      // Ticket exists, but user does not have permissions to it.
      final case object TicketGetIllegalAccess extends GetError

      sealed trait UpdateError

      final case object UpdateTicketNotExists extends UpdateError

      // User can get, but can't modify tickets
      final case object UpdateTicketIllegalAccess extends UpdateError

      // Somebody else already modified ticket, while user was working
      final case object UpdateTicketConcurrentModification extends UpdateError
    }

    """compose functionality such as "update status" e.g. get ticket; change status; update""" in {
      import cats.Monad
      import cats.implicits._

      abstract class TS[F[_]: Monad] extends TicketsService[F] {
        def updateStatus(id: String, currentUser: User, toStatus: String) = {
          for {
            ticket <- getTicket(id, currentUser)
            updatedTicket = ticket.copy(status = toStatus)
            // The following would result in a Compilation error:
            // type mismatch, because EitherT[F, GetError, _] (from getTicket) and EitherT[F, UpdateError, _] (from updateTicket) have different left type
            // _ <- updateTicket(updatedTicket, currentUser)
          } yield ()
        }
      }

      // How to resolve the above issue?

      // We could introduce a common ancestor for instance and return more general error.
      // Yes, but this does not sound like a good way to go - What if later we will need another errors' composition?
    }
  }

  /**
   * Instead of having hierarchical structure of errors per operation — we can have flattened general purpose error models, which then compose per operation via Coproduct
   */
  "Shapeless Coproduct" should {
    import TicketsService._
    import cats.effect.Sync
    import shapeless._

    final case class User(id: String)

    final case class Ticket(id: String, status: String, lastUpdatedUser: User, lastUpdateTimestamp: Long)

    abstract class TicketsService[F[_]: Sync] {
      final type Result[E, R] = EitherT[F, E, R]

      def getTicket(id: String, currentUser: User): Result[GetError, Ticket]

      def updateTicket(update: Ticket, currentUser: User): Result[UpdateError, Ticket]
    }

    /**
     * Instead of GetError and UpdateError sum types, we have now aliases for Coproduct of errors.
     *
     * We don’t need to duplicated similar "ticket not exists" and "ticket illegal access" objects in different hierarchies, because structure of low-lever errors now flat.
     */
    object TicketsService {
      sealed trait Operation

      final case object Get extends Operation

      final case object Update extends Operation

      case object TicketNotExists

      case class TicketIllegalAccess(operation: Operation)

      case object ConcurrentModification

      type GetError = TicketNotExists.type :+: TicketIllegalAccess :+: CNil

      type UpdateError = TicketNotExists.type :+: TicketIllegalAccess :+: ConcurrentModification.type :+: CNil
    }

    """composition of coproducts such as "update status" e.g. get ticket; change status; update""" in {
      import cats.{Functor, Inject}
      import cats.implicits._

      // TODO - Should compile but does not
      /*implicit class CoproductEitherT[F[_]: Functor, A, B](underlying: EitherT[F, A, B]) {
        /**
         * Wraps left into coproduct, so to align two 'Either' to common ancestor
         */
        def widenLeft[A1 <: Coproduct](implicit inj: Inject[A1, A]): EitherT[F, A1, B] = underlying.leftMap(Coproduct[A1](_))
      }

      abstract class TS[F[_]: Sync] extends TicketsService[F] {
        type UpdateStatusError = GetError :+: UpdateError :+: CNil

        def updateStatus(id: String, currentUser: User, toStatus: String): EitherT[F, UpdateStatusError, Unit] = {
          for {
            // Turning 'GetError' to wider 'UpdateStatusError' coproduct - similar to re-throwing error
            ticket <- getTicket(id, currentUser).widenLeft[UpdateStatusError]
            updatedTicket = ticket.copy(status = toStatus)
            // Turning `UpdateError` to wider `UpdateStatusError` coproduct - similar to re-throwing error
            _ <- updateTicket(updatedTicket, currentUser).widenLeft[UpdateStatusError]
          } yield ()
        }
      }*/
    }
  }
}