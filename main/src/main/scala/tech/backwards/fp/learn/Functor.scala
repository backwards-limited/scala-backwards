package tech.backwards.fp.learn

trait Functor[F[_]] {
  def fmap[A, B](fa: F[A])(f: A => B): F[B]
}

object Functor extends FunctorImplicits {
  def apply[F[_]: Functor]: Functor[F] =
    implicitly

  object syntax {
    implicit class FunctorSyntax[F[_]: Functor, A](fa: F[A]) {
      def fmap[B](f: A => B): F[B] =
        apply[F].fmap(fa)(f)

      def map[B](f: A => B): F[B] =
        fmap(f)

      def `<$>`[B](f: A => B): F[B] =
        fmap(f)
    }

    object function {
      implicit class FunctorSyntax[A, B](f: A => B) {
        def fmap[F[_]: Functor](fa: F[A]): F[B] =
          apply[F].fmap(fa)(f)

        def map[F[_]: Functor](fa: F[A]): F[B] =
          fmap(fa)

        def `<$>`[F[_]: Functor](fa: F[A]): F[B] =
          fmap(fa)
      }
    }
  }
}

sealed trait FunctorImplicits {
  implicit val functorList: Functor[List] =
    new Functor[List] {
      def fmap[A, B](fa: List[A])(f: A => B): List[B] =
        fa map f
    }
}