package tech.backwards.fp.learn

sealed trait Disjunction[+L, +R]

final case class Left[L, R] private(value: L) extends Disjunction[L, R]

object Left {
  def apply[L, R](l: L): Disjunction[L, R] =
    new Left(l)
}

final case class Right[L, R] private(value: R) extends Disjunction[L, R]

object Right {
  def apply[L, R](r: R): Disjunction[L, R] =
    new Right(r)
}

object Disjunction {
  object syntax {
    implicit class DisjunctionSyntax[A](a: A) {
      def right[L]: Disjunction[L, A] =
        Right[L, A](a)

      def left[R]: Disjunction[A, R] =
        Left[A, R](a)
    }
  }

  /**
   * By using the "kind projector" compiler plugin the following becomes much easier:
   * {{{
   *  implicit def functorDisjunction[L] =
   *    new Functor[({ type E[A] = Disjunction[L, A] })# E]
   * }}}
   */
  implicit def functorDisjunction[L]: Functor[Disjunction[L, *]] =
    new Functor[Disjunction[L, *]] {
      def fmap[A, B](fa: Disjunction[L, A])(f: A => B): Disjunction[L, B] =
        fa match {
          case Left(l) =>
            Left(l)

          case Right(a) =>
            Right(f(a))
        }
    }

  implicit def applicativeDisjunction[L]: Applicative[Disjunction[L, *]] =
    new Applicative[Disjunction[L, *]] {
      def pure[A](a: A): Disjunction[L, A] =
        Right(a)

      def ap[A, B](ff: Disjunction[L, A => B])(fa: Disjunction[L, A]): Disjunction[L, B] =
        ff match {
          case Left(l) =>
            Left(l)

          case Right(f) =>
            Functor[Disjunction[L, *]].fmap(fa)(f)
        }
    }

  /**
   * By using the "kind projector" compiler plugin the following becomes much easier:
   * {{{
   *  implicit def monadDisjunction[L] =
   *    new Monad[({ type E[A] = Disjunction[L, A] })# E]
   * }}}
   */
  implicit def monadDisjunction[L]: Monad[Disjunction[L, *]] =
    new Monad[Disjunction[L, *]] {
      def pure[A](a: A): Disjunction[L, A] =
        Right(a)

      def flatMap[A, B](fa: Disjunction[L, A])(f: A => Disjunction[L, B]): Disjunction[L, B] =
        fa match {
          case Left(l) =>
            Left(l)

          case Right(a) =>
            f(a)
        }
    }

  /**
   * By using the "kind projector" compiler plugin the following becomes much easier:
   * {{{
   *   implicit def foldableDisjunction[L] =
   *   new Foldable[({ type E[A] = Disjunction[L, A] })# E]
   * }}}
   */
  implicit def foldableDisjunction[L]: Foldable[Disjunction[L, *]] =
    new Foldable[Disjunction[L, *]] {
      def foldr[A, B](fa: Disjunction[L, A])(seed: B)(f: (A, B) => B): B =
        fa match {
          case Left(_) =>
            seed

          case Right(a) =>
            f(a, seed)
        }
    }

  implicit def traversalDisjunction[L]: Traversal[Disjunction[L, *]] =
    new Traversal[Disjunction[L, *]] {
      def traverse[G[_]: Applicative, A, B](fa: Disjunction[L, A])(f: A => G[B]): G[Disjunction[L, B]] =
        fa match {
          case Left(l) =>
            Applicative[G].pure(Left(l))

          case Right(a) =>
            Applicative[G].functor.fmap(f(a))(b => Right(b))
        }
    }
}