package tech.backwards.fp.learn

sealed abstract case class IO[A] private(thunk: () => A) {
  def unsafeRunSync(): A =
    thunk()
}

object IO {
  def apply[A](thunk: => A): IO[A] =
    new IO(() => thunk) {}

  implicit val functorIO: Functor[IO] =
    new Functor[IO] {
      def fmap[A, B](fa: IO[A])(f: A => B): IO[B] =
        IO(f(fa.unsafeRunSync()))
    }

  implicit val monadIO: Monad[IO] =
    new Monad[IO] {
      def pure[A](a: A): IO[A] =
        IO(a)

      def flatMap[A, B](fa: IO[A])(f: A => IO[B]): IO[B] =
        f(fa.unsafeRunSync())
    }

  implicit val applicativeIO: Applicative[IO] =
    new Applicative[IO] {
      def pure[A](a: A): IO[A] =
        monadIO.pure(a)

      def ap[A, B](ff: IO[A => B])(fa: IO[A]): IO[B] =
        functorIO.fmap(fa)(ff.unsafeRunSync())
    }
}