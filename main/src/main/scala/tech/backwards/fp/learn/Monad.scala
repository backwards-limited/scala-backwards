package tech.backwards.fp.learn

abstract class Monad[F[_]: Functor] {
  def pure[A](a: A): F[A]

  def flatMap[A, B](fa: F[A])(f: A => F[B]): F[B]
}

object Monad {
  def apply[F[_]: Monad]: Monad[F] =
    implicitly

  object syntax {
    implicit class MonadSyntax[F[_]: Monad, A](fa: F[A]) {
      def flatMap[B](f: A => F[B]): F[B] =
        apply[F].flatMap(fa)(f)

      def >>=[B](f: A => F[B]): F[B] =
        flatMap(f)
    }
  }
}