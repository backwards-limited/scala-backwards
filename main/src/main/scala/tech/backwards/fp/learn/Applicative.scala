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
  implicit val applicativeList: Applicative[List] =
    new Applicative[List] {
      def pure[A](a: A): List[A] =
        List(a)

      def ap[A, B](ff: List[A => B])(fa: List[A]): List[B] =
        ff flatMap fa.map

      /* Alternative though not that nice:
      def ap[A, B](ff: List[A => B])(fa: List[A]): List[B] =
        ff match {
          case f :: fs =>
            fa.map(f) ++ ap(fs)(fa)

          case _ =>
            Nil
        }
      */
    }
}