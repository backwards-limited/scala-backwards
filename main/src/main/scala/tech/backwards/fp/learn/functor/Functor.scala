package tech.backwards.fp.learn.functor

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

      def `<$>`[B](f: A => B): F[B] =
        fmap(f)
    }

    implicit class FunctionSyntax[A, B](f: A => B) {
      def fmap[F[_]: Functor](fa: F[A]): F[B] =
        apply[F].fmap(fa)(f)

      def `<$>`[F[_]: Functor](fa: F[A]): F[B] =
        fmap(fa)
    }
  }
}

sealed trait FunctorImplicits {
  implicit val functorId: Functor[Id] =
    new Functor[Id] {
      def fmap[A, B](fa: Id[A])(f: A => B): Id[B] =
        Id(f(fa.value))
    }
}