package tech.backwards.fp.learn

import tech.backwards.fp.learn.Disjunction.syntax._

final case class DisjunctionT[F[_], L, R](value: F[L Disjunction R])

object DisjunctionT {
  def pure[F[_]: Applicative, L, R](r: R): DisjunctionT[F, L, R] =
    DisjunctionT(Applicative[F].pure(r.right[L]))

  def lift[F[_]: Functor, L, R](fa: F[R]): DisjunctionT[F, L, R] =
    DisjunctionT(Functor[F].fmap(fa)(_.right))

  implicit def functorDisjunctionT[F[_]: Functor, L]: Functor[DisjunctionT[F, L, *]] =
    new Functor[DisjunctionT[F, L, *]] {
      import tech.backwards.fp.learn.Functor.syntax._

      def fmap[A, B](fa: DisjunctionT[F, L, A])(f: A => B): DisjunctionT[F, L, B] =
        DisjunctionT(Functor[F].fmap(fa.value)(_.fmap(f)))
    }

  implicit def applicativeDisjunctionT[F[_]: Functor: Applicative, L]: Applicative[DisjunctionT[F, L, *]] =
    new Applicative[DisjunctionT[F, L, *]] {
      import tech.backwards.fp.learn.Functor.syntax._
      import tech.backwards.fp.learn.Applicative.syntax._

      def pure[A](a: A): DisjunctionT[F, L, A] =
        DisjunctionT(Applicative[F].pure(Right(a)))

      def ap[A, B](ff: DisjunctionT[F, L, A => B])(fa: DisjunctionT[F, L, A]): DisjunctionT[F, L, B] =
        DisjunctionT(
          ff.value `<$>` ((ff: L Disjunction (A => B)) => (fa: L Disjunction A) => ff <*> fa) <*> fa.value
        )
    }

  implicit def monadDisjunctionT[F[_]: Functor: Monad, L]: Monad[DisjunctionT[F, L, *]] =
    new Monad[DisjunctionT[F, L, *]] {
      import tech.backwards.fp.learn.Monad.syntax._

      def pure[A](a: A): DisjunctionT[F, L, A] =
        DisjunctionT(Monad[F].pure(Right[L, A](a)))

      def flatMap[A, B](fa: DisjunctionT[F, L, A])(f: A => DisjunctionT[F, L, B]): DisjunctionT[F, L, B] =
        DisjunctionT(
          fa.value.flatMap {
            case Left(l)  => Monad[F].pure(Left[L, B](l))
            case Right(a) => f(a).value
          }
        )
    }
}
