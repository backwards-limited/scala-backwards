package tech.backwards.fp.foldable

import scala.language.implicitConversions

trait Foldable[F[_]] {
  def foldr[A, B](fa: F[A])(acc: B)(f: (A, B) => B): B
}

object Foldable {
  def apply[F[_]: Foldable]: Foldable[F] =
    implicitly
}

class FoldableOps[F[_]: Foldable, A](fa: F[A]) {
  def foldr[B](acc: B)(f: (A, B) => B): B =
    Foldable[F].foldr(fa)(acc)(f)
}

object FoldableOps {
  implicit def toFoldableOps[F[_]: Foldable, A](fa: F[A]): FoldableOps[F, A] =
    new FoldableOps(fa)

  /**
    * Because of using the "kind projector" compiler plugin the following becomes much easier:
    * {{{
    *   implicit def tuple2ToFoldableOps[A](fa: (A, A))(implicit FOLDABLE: Foldable[({ type E[X] = (X, X) })# E]) =
    *     new FoldableOps[({ type E[X] = (X, X) })# E, A](fa)
    * }}}
    */
  implicit def tuple2ToFoldableOps[A](fa: (A, A))(implicit Foldable: Foldable[Lambda[X => (X, X)]]): FoldableOps[Lambda[X => (X, X)], A] =
    new FoldableOps[Lambda[X => (X, X)], A](fa)

  /**
    * Because of using the "kind projector" compiler plugin the following becomes much easier:
    * {{{
    *   implicit def tuple3ToFoldableOps[A](fa: (A, A, A))(implicit FOLDABLE: Foldable[({ type E[X] = (X, X, X) })# E]) =
    *     new FoldableOps[({ type E[X] = (X, X, X) })# E, A](fa)
    * }}}
    */
  implicit def tuple3ToFoldableOps[A](fa: (A, A, A))(implicit Foldable: Foldable[Lambda[X => (X, X, X)]]): FoldableOps[Lambda[X => (X, X, X)], A] =
    new FoldableOps[Lambda[X => (X, X, X)], A](fa)
}