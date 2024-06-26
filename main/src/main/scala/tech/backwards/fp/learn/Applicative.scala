package tech.backwards.fp.learn

abstract class Applicative[F[_]: Functor] {
  val functor: Functor[F] =
    Functor[F]

  def pure[A](a: A): F[A]

  def ap[A, B](ff: F[A => B])(fa: F[A]): F[B]

  def map2[A, B, C](fa: F[A], fb: F[B])(f: (A, B) => C): F[C] =
    ap(ap(pure((a: A) => (b: B) => f(a, b)))(fa))(fb)
}

object Applicative {
  def apply[F[_]: Applicative]: Applicative[F] =
    implicitly

  // Or simply Applicative[F].map2(fa, fb)(f), but don't actually need the following function
  def map2[F[_]: Functor: Applicative, A, B, C](fa: F[A], fb: F[B])(f: (A, B) => C): F[C] =
    Applicative[F].ap(Applicative[F].ap(Applicative[F].pure((a: A) => (b: B) => f(a, b)))(fa))(fb)

  object syntax {
    implicit class ApplicativeSyntax[A](a: A) {
      def pure[F[_] : Applicative]: F[A] =
        apply[F].pure(a)
    }

    implicit class ApplicativeTypeConstructorSyntax[F[_]: Applicative, A](fa: F[A]) {
      def ap[B](ff: F[A => B]): F[B] =
        apply[F].ap(ff)(fa)

      def <*>[B](ff: F[A => B]): F[B] =
        ap(ff)
    }

    implicit class ApplicativeTypeFunctionConstructorSyntax[F[_]: Applicative, A, B](ff: F[A => B]) {
      def ap(fa: F[A]): F[B] =
        apply[F].ap(ff)(fa)

      def <*>(fa: F[A]): F[B] =
        ap(fa)
    }
  }

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

  implicit def applicativeTuple1: Applicative[Tuple1] =
    new Applicative[Tuple1] {
      def pure[A](a: A): Tuple1[A] =
        Tuple1(a)

      def ap[A, B](ff: Tuple1[A => B])(fa: Tuple1[A]): Tuple1[B] =
        Tuple1(ff._1(fa._1))
    }

  implicit def applicativeTuple2: Applicative[Lambda[X => (X, X)]] =
    new Applicative[Lambda[X => (X, X)]] {
      def pure[A](a: A): (A, A) =
        a -> a

      def ap[A, B](ff: (A => B, A => B))(fa: (A, A)): (B, B) =
        ff._1(fa._1) -> ff._2(fa._2)
    }

  implicit def applicativeTuple3: Applicative[Lambda[X => (X, X, X)]] =
    new Applicative[Lambda[X => (X, X, X)]] {
      def pure[A](a: A): (A, A, A) =
        (a, a, a)

      def ap[A, B](ff: (A => B, A => B, A => B))(fa: (A, A, A)): (B, B, B) =
        (ff._1(fa._1), ff._2(fa._2), ff._3(fa._3))
    }
}