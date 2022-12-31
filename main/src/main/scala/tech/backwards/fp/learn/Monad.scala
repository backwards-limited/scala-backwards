package tech.backwards.fp.learn

abstract class Monad[F[_]: Functor] {
  def pure[A](a: A): F[A]

  def flatMap[A, B](fa: F[A])(f: A => F[B]): F[B]
}

object Monad extends MonadImplicits {
  def apply[F[_]: Monad]: Monad[F] =
    implicitly

  object syntax extends LowerLevelImplicits {
    implicit class MonadSyntax[A](a: A) {
      def pure[F[_]: Monad]: F[A] =
        apply[F].pure(a)
    }

    object function {
      implicit class MonadSyntax[F[_]: Monad, A, B](f: A => F[B]) {
        def flatMap(fa: F[A]): F[B] =
          apply[F].flatMap(fa)(f)

        def >>=(fa: F[A]): F[B] =
          flatMap(fa)
      }
    }
  }

  trait LowerLevelImplicits {
    implicit class MonadSyntax[F[_]: Monad, A](fa: F[A]) {
      def flatMap[B](f: A => F[B]): F[B] =
        apply[F].flatMap(fa)(f)

      def >>=[B](f: A => F[B]): F[B] =
        flatMap(f)
    }
  }
}

trait MonadImplicits {
  implicit val monadList: Monad[List] =
    new Monad[List] {
      def pure[A](a: A): List[A] =
        List(a)

      def flatMap[A, B](fa: List[A])(f: A => List[B]): List[B] =
        fa.flatMap(f)

      /*
      We could accumulate manually, but we'll just reuse List.flatMap as above.
      def flatMap[A, B](fa: List[A])(f: A => List[B]): List[B] = {
        def go(fb: List[B]): List[A] => List[B] = {
          case a :: as =>
            go(fb ::: f(a))(as)

          case Nil =>
            fb
        }

        go(List.empty[B])(fa)
      }
      */
    }
}