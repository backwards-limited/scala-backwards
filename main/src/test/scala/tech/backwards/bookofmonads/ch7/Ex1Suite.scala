package tech.backwards.bookofmonads.ch7

import scala.annotation.tailrec
import cats.{Functor, Monad}
import cats.implicits._
import munit.FunSuite

/**
 * Failure and Logic
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

  test("Either Monad") {
    type Error = String

    implicit def monadEither[L]: Monad[L Either *] =
      new Monad[L Either *] {
        def pure[A](a: A): L Either A =
          a.asRight

        def flatMap[A, B](fa: L Either A)(f: A => L Either B): L Either B =
          fa match {
            case Left(e) => e.asLeft
            case Right(a) => f(a)
          }

        @tailrec
        def tailRecM[A, B](a: A)(f: A => L Either (A Either B)): L Either B =
          f(a) match {
            case Left(e) => e.asLeft
            case Right(Left(a)) => tailRecM(a)(f)
            case Right(Right(b)) => b.asRight
          }
      }

    val result: Error Either Int =
      Monad[Error Either *].flatMap(5.asRight)(i => (i + 5).asRight)

    assertEquals(
      result,
      10.asRight
    )
  }
}