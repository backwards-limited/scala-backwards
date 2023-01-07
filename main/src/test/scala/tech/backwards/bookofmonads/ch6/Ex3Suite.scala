package tech.backwards.bookofmonads.ch6

import cats.data.WriterT
import cats.effect.IO
import cats.implicits.{catsSyntaxFlatMapOps, catsSyntaxOptionId, none}
import weaver._

/**
 * Writer Monad
 */
object Ex3Suite extends SimpleIOSuite {
  test("Writer") {
    final case class User(id: Int)

    final case class Order(totalAmount: Int)

    def user(id: Int): WriterT[IO, Vector[String], User] =
      // The apply method is a simple way of constructing a writer with a log and result, suspended in F
      WriterT(IO(Vector("Getting user") -> User(id)))

    def order(user: User): WriterT[IO, Vector[String], Order] =
      WriterT(IO(Vector(s"Getting order for user: $user") -> Order(100)))

    val program: WriterT[IO, Vector[String], Order] =
      user(1) >>= order

    program.run.map(xs => expect(xs == Vector("Getting user", "Getting order for user: User(1)") -> Order(100)))
  }
}