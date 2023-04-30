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
}