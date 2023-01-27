package tech.backwards.fp.learn

abstract class Applicative[F[_]: Functor] {
  def pure[A](a: A): F[A]

  def ap[A, B](ff: F[A => B])(fa: F[A]): F[B]
}

object Applicative extends ApplicativeImplicits {
  def apply[F[_]: Applicative]: Applicative[F] =
    implicitly

  object syntax {
    implicit class Syntax[A](a: A) {
      def pure[F[_]: Applicative]: F[A] =
        apply[F].pure(a)
    }

    object function {
      implicit class ApplicativeSyntax[F[_]: Applicative, A, B](ff: F[A => B]) {
        def ap(fa: F[A]): F[B] =
          apply[F].ap(ff)(fa)

        def <*>(fa: F[A]): F[B] =
          ap(fa)
      }
    }
  }
}

sealed trait ApplicativeImplicits {
  /*implicit val monadList: Monad[List] =
    new Monad[List] {
      def pure[A](a: A): List[A] =
        List(a)

      def flatMap[A, B](fa: List[A])(f: A => List[B]): List[B] =
        fa.flatMap(f)
    }*/
}