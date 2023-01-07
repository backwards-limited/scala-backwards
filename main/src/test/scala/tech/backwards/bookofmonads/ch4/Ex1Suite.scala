package tech.backwards.bookofmonads.ch4

import cats.{Applicative, Functor, Monad}
import cats.effect._
import cats.implicits._
import scribe.Logger
import tech.backwards.scribe.TestLogHandler
import weaver._

/**
 * Utilities for monadic code
 */
object Ex1Suite extends SimpleIOSuite {
  pureTest("Functor map") {
    expect.eql(
      Functor[List].map(List(1, 2, 3))(_ + 1),
      List(2, 3, 4)
    )

    def map[F[_] : Functor, A, B](f: A => B)(Fa: F[A]): F[B] =
      Functor[F].map(Fa)(f)

    expect.eql(
      map((_: Int) + 1)(List(1, 2, 3)),
      List(2, 3, 4)
    )
  }

  test("Monadic action will do nothing when mapped over - we'll end up with a list of monadic action instead of a monadic action itself") {
    val testLogHandler: TestLogHandler =
      new TestLogHandler

    val logger: Logger =
      Logger("Blah").withHandler(testLogHandler)

    val result: List[IO[Unit]] =
      Functor[List].map(List("Bob", "Sue"))(name => IO(logger info name))
      // We end up with a list of monadic actions, not a monadic action itself.

    result.sequence.map(xs =>
      expect.all(
        xs == List.fill(2)(()),
        testLogHandler.logRecords.map(_.logOutput.plainText) == List("Bob", "Sue")
      )
    )
  }

  test("Monadic action first resolution") {
    def mapM[F[_]: Monad, A, B](f: A => F[B])(xs: List[A]): F[List[B]] =
      xs match {
        case Nil =>
          Monad[F].pure(Nil)

        case x :: xs =>
          Monad[F].flatMap(f(x)) { b =>
            Monad[F].map(mapM(f)(xs))(bs => b :: bs)
          }
      }

    val testLogHandler: TestLogHandler =
      new TestLogHandler

    val logger: Logger =
      Logger("Blah").withHandler(testLogHandler)

    val result: IO[List[Unit]] =
      mapM((name: String) => IO(logger info name))(List("Bob", "Sue"))

    result.map(xs =>
      expect.all(
        xs == List.fill(2)(()),
        testLogHandler.logRecords.map(_.logOutput.plainText) == List("Bob", "Sue")
      )
    )
  }

  test("Monadic action first resolution - applicative version") {
    def mapM[F[_]: Monad, A, B](f: A => F[B])(xs: List[A]): F[List[B]] =
      xs match {
        case Nil =>
          Monad[F].pure(Nil)

        case x :: xs =>
          // Haskell: mapM f (x : xs) = (:) <$> f x <*> mapM f xs
          Applicative[F].ap(Functor[F].map(f(x))(a => a :: (_: List[B])))(mapM(f)(xs))
      }

    val testLogHandler: TestLogHandler =
      new TestLogHandler

    val logger: Logger =
      Logger("Blah").withHandler(testLogHandler)

    val result: IO[List[Unit]] =
      mapM((name: String) => IO(logger info s"Hi $name"))(List("Bob", "Sue"))

    result.map(xs =>
      expect.all(
        xs == List.fill(2)(()),
        testLogHandler.logRecords.map(_.logOutput.plainText) == List("Hi Bob", "Hi Sue")
      )
    )
  }

  test("sequence, which leads to redefining mapM in terms of sequence") {
    def sequence[F[_]: Monad, A](xs: List[F[A]]): F[List[A]] =
      xs match {
        case Nil =>
          Monad[F].pure(Nil)

        case x :: xs =>
          Monad[F].flatMap(x)(a =>
            Monad[F].map(sequence(xs))(as =>
              a :: as
            )
          )
      }

    sequence(List(IO("a"), IO("b"))).map(xs => expect(xs == List("a", "b")))
  }

  test("sequence, which leads to redefining mapM in terms of sequence - Applicative style") {
    def sequence[F[_]: Monad, A](xs: List[F[A]]): F[List[A]] =
      xs match {
        case Nil =>
          Monad[F].pure(Nil)

        case x :: xs =>
          // Haskell: sequence (x : xs) = (:) <$> x <*> sequence xs
          Applicative[F].ap(Functor[F].map(x)(a => a :: (_: List[A])))(sequence(xs))
      }

    sequence(List(IO("a"), IO("b"))).map(xs => expect(xs == List("a", "b")))
  }

  test("Redefine mapM as a composition of sequence and map") {
    def sequence[F[_]: Monad, A](xs: List[F[A]]): F[List[A]] =
      xs match {
        case Nil =>
          Monad[F].pure(Nil)

        case x :: xs =>
          // Haskell: sequence (x : xs) = (:) <$> x <*> sequence xs
          Applicative[F].ap(Functor[F].map(x)(a => a :: (_: List[A])))(sequence(xs))
      }

    def mapM[F[_]: Monad, A, B](f: A => F[B])(xs: List[A]): F[List[B]] =
      sequence(xs.map(f))

    val result: IO[List[String]] =
      mapM((name: String) => IO(s"Hi $name"))(List("Bob", "Sue"))

    result.map(xs => expect(xs == List("Hi Bob", "Hi Sue")))
  }

  test("zipWithM") {
    def sequence[F[_]: Monad, A](xs: List[F[A]]): F[List[A]] =
      xs match {
        case Nil =>
          Monad[F].pure(Nil)

        case x :: xs =>
          Applicative[F].ap(Functor[F].map(x)(a => a :: (_: List[A])))(sequence(xs))
      }

    // Haskell:
    // zipWithM :: Monad m => (a -> b -> m c) -> [a] -> [b] -> m [c]
    def zipWithM[F[_]: Monad, A, B, C](f: A => B => F[C])(as: List[A])(bs: List[B]): F[List[C]] = {
      val v: List[(A, B)] =
        as.zip(bs)

      val z: List[F[C]] =
        v.map(Function.uncurried(f).tupled)

      sequence(z)
    }

    val result: IO[List[String]] =
      zipWithM((a: Int) => (b: String) => IO(b + a))(List(1, 2))(List("x", "y"))

    result.map(xs => expect(xs == List("x1", "y2")))
  }

  test("zipWithM - Shorter version") {
    def sequence[F[_]: Monad, A](xs: List[F[A]]): F[List[A]] =
      xs match {
        case Nil =>
          Monad[F].pure(Nil)

        case x :: xs =>
          Applicative[F].ap(Functor[F].map(x)(a => a :: (_: List[A])))(sequence(xs))
      }

    def zipWithM[F[_]: Monad, A, B, C](f: A => B => F[C])(as: List[A])(bs: List[B]): F[List[C]] =
      sequence(as.zip(bs) map Function.uncurried(f).tupled)

    val result: IO[List[String]] =
      zipWithM((a: Int) => (b: String) => IO(b + a))(List(1, 2))(List("x", "y"))

    result.map(xs => expect(xs == List("x1", "y2")))
  }

  test("replicateM") {
    def sequence[F[_]: Monad, A](xs: List[F[A]]): F[List[A]] =
      xs match {
        case Nil =>
          Monad[F].pure(Nil)

        case x :: xs =>
          Applicative[F].ap(Functor[F].map(x)(a => a :: (_: List[A])))(sequence(xs))
      }

    // Haskell
    // replicateM :: Monad m => Int -> m a -> m [a]
    def replicateM[F[_]: Monad, A](n: Int)(Fa: F[A]): F[List[A]] =
      sequence(List.fill(n)(Fa))

    replicateM(3)(IO("hi")).map(xs => expect(xs == List("hi", "hi", "hi")))
  }
}