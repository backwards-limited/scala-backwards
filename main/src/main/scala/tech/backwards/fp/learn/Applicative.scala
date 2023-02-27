package tech.backwards.fp.learn

abstract class Applicative[F[_]: Functor] {
  val functor: Functor[F] =
    Functor[F]

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

  implicit def applicativeTuple1(implicit F: Functor[Tuple1]): Applicative[Tuple1] =
    new Applicative[Tuple1] {
      def pure[A](a: A): Tuple1[A] =
        Tuple1(a)

      def ap[A, B](ff: Tuple1[A => B])(fa: Tuple1[A]): Tuple1[B] =
        Tuple1(ff._1(fa._1))
    }

  implicit def applicativeTuple2(implicit F: Functor[Lambda[X => (X, X)]]): Applicative[Lambda[X => (X, X)]] =
    new Applicative[Lambda[X => (X, X)]] {
      def pure[A](a: A): (A, A) =
        a -> a

      def ap[A, B](ff: (A => B, A => B))(fa: (A, A)): (B, B) =
        ff._1(fa._1) -> ff._2(fa._2)
    }

  implicit def applicativeTuple3(implicit F: Functor[Lambda[X => (X, X, X)]]): Applicative[Lambda[X => (X, X, X)]] =
    new Applicative[Lambda[X => (X, X, X)]] {
      def pure[A](a: A): (A, A, A) =
        (a, a, a)

      def ap[A, B](ff: (A => B, A => B, A => B))(fa: (A, A, A)): (B, B, B) =
        (ff._1(fa._1), ff._2(fa._2), ff._3(fa._3))
    }
}