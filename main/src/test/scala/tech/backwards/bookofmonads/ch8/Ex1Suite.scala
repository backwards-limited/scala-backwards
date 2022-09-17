package tech.backwards.bookofmonads.ch8

import scala.annotation.tailrec
import cats.implicits._
import cats.{Functor, Monad, MonadError}
import munit.FunSuite

/**
 * Monads for Mutability
 */
class Ex1Suite extends FunSuite {
  test("Either Functor") {
    type Error = String

    implicit def functorEither[L]: Functor[L Either *] =
      new Functor[L Either *] {
        def map[A, B](fa: L Either A)(f: A => B): L Either B =
          fa match {
            case Left(e) => e.asLeft
            case Right(a) => f(a).asRight
          }
      }

    val result: Error Either Int =
      Functor[Error Either *].map(5.asRight)(_ + 5)

    assertEquals(
      result,
      10.asRight
    )
  }
}