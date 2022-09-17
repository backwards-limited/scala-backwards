package tech.backwards.bookofmonads.ch7

import scala.annotation.tailrec
import cats.{Functor, Monad, MonadError}
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

  test("MonadError instance for Option") {
    implicit val monadErrorOption: MonadError[Option, Unit] =
      new MonadError[Option, Unit] {
        def flatMap[A, B](Fa: Option[A])(f: A => Option[B]): Option[B] =
          Fa.flatMap(f)

        def tailRecM[A, B](a: A)(f: A => Option[Either[A, B]]): Option[B] =
          f(a).fold(none[B]) {
            case Left(a) => tailRecM(a)(f)
            case Right(b) => b.some
          }

        def raiseError[A](e: Unit): Option[A] =
          none

        def handleErrorWith[A](Fa: Option[A])(f: Unit => Option[A]): Option[A] =
          Fa match {
            case Some(a) => a.some
            case _ => f(())
          }

        def pure[A](x: A): Option[A] =
          x.some
      }

    def example[F[_]: MonadError[*[_], Unit], A](Fa: F[A])(f: A => Boolean): F[Boolean] =
      MonadError[F, Unit].map(Fa)(f).flatMap(b =>
        if (b) MonadError[F, Unit].pure(b) else MonadError[F, Unit].raiseError(())
      )

    assertEquals(
      example(5.some)(_ > 4),
      true.some
    )

    assertEquals(
      example(5.some)(_ > 5),
      none
    )
  }

  test("MonadError instance for Either") {
    sealed trait BusinessError

    final case object SeriousError extends BusinessError

    final case object MarginalError extends BusinessError

    implicit def monadErrorEither[E]: MonadError[Either[E, *], E] =
      new MonadError[Either[E, *], E] {
        def flatMap[A, B](Fa: Either[E, A])(f: A => Either[E, B]): Either[E, B] =
          Fa.flatMap(f)

        def tailRecM[A, B](a: A)(f: A => Either[E, Either[A, B]]): Either[E, B] =
          f(a).fold(_.asLeft, {
            case Left(a) => tailRecM(a)(f)
            case Right(b) => b.asRight
          })

        def raiseError[A](e: E): Either[E, A] =
          e.asLeft

        def handleErrorWith[A](Fa: Either[E, A])(f: E => Either[E, A]): Either[E, A] =
          Fa.leftFlatMap(f)

        def pure[A](x: A): Either[E, A] =
          x.asRight
      }

    def example[F[_] : MonadError[*[_], BusinessError], A](Fa: F[A])(f: A => Boolean): F[Boolean] =
      MonadError[F, BusinessError].map(Fa)(f).flatMap(b =>
        if (b) MonadError[F, BusinessError].pure(b) else MonadError[F, BusinessError].raiseError(SeriousError)
      )

    assertEquals(
      example(5.asRight[BusinessError])(_ > 4),
      true.asRight
    )

    assertEquals(
      example(5.asRight[BusinessError])(_ > 5),
      SeriousError.asLeft
    )
  }
}