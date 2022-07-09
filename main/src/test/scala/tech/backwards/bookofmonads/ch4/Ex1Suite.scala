package tech.backwards.bookofmonads.ch4

import cats.Functor
import cats.effect.IO
import cats.implicits.{catsSyntaxOptionId, none}
import munit._
import scribe.Logger
import tech.backwards.scribe.TestLogHandler

/**
 * Utilities for monadic code
 */
class Ex1Suite extends FunSuite {
  test("Functor map") {
    assertEquals(
      Functor[List].map(List(1, 2, 3))(_ + 1),
      List(2, 3, 4)
    )

    def map[F[_]: Functor, A, B](f: A => B)(Fa: F[A]): F[B] =
      Functor[F].map(Fa)(f)

    assertEquals(
      map((_: Int) + 1)(List(1, 2, 3)),
      List(2, 3, 4)
    )
  }

  test("Monadic action will do nothing when mapped over") {
    val testLogHandler: TestLogHandler =
      new TestLogHandler

    val logger: Logger =
      Logger("Blah").withHandler(testLogHandler)

    Functor[List].map(List("Bob", "Sue"))(name => IO(logger.info(name)))

    assertEquals(
      testLogHandler.logRecords,
      Nil
    )
  }
}