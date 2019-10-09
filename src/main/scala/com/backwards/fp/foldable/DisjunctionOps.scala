package com.backwards.fp.foldable

import scala.language.higherKinds
import com.backwards.fp.{Disjunction, LeftDisjunction, RightDisjunction}

object DisjunctionOps {
  /**
    * Because of using the "kind projector" compiler plugin the following becomes much easier:
    * {{{
    *   implicit def disjunctionFoldable[L] = new Foldable[({ type E[X] = Disjunction[L, X] })# E]
    * }}}
    */
  implicit def disjunctionFoldable[L]: Foldable[Disjunction[L, *]] = new Foldable[Disjunction[L, *]] {
    def foldr[A, B](fa: Disjunction[L, A])(acc: B)(f: (A, B) => B): B = fa match {
      case r @ RightDisjunction(_) => rightDisjunctionFoldable.foldr(r)(acc)(f)
      case l @ LeftDisjunction(_) => leftDisjunctionFoldable.foldr(l)(acc)(f)
    }
  }

  /**
    * Because of using the "kind projector" compiler plugin the following becomes much easier:
    * {{{
    *   implicit def rightDisjunctionFoldable[L, R] = new Foldable[({ type E[X] = RightDisjunction[L, X] })# E]
    * }}}
    */
  implicit def rightDisjunctionFoldable[L, R]: Foldable[RightDisjunction[L, *]] = new Foldable[RightDisjunction[L, *]] {
    def foldr[A, B](fa: RightDisjunction[L, A])(acc: B)(f: (A, B) => B): B = f(fa.value, acc)
  }

  /**
    * Because of using the "kind projector" compiler plugin the following becomes much easier:
    * {{{
    *   implicit def leftDisjunctionFoldable[L, R] = new Foldable[({ type E[X] = LeftDisjunction[L, X] })# E]
    * }}}
    */
  implicit def leftDisjunctionFoldable[L, R]: Foldable[LeftDisjunction[L, *]] = new Foldable[LeftDisjunction[L, *]] {
    def foldr[A, B](fa: LeftDisjunction[L, A])(acc: B)(f: (A, B) => B): B = acc
  }

  /**
    * Because of using the "kind projector" compiler plugin the following becomes much easier:
    * {{{
    *   implicit def toFoldableOps[D[L, R] <: Disjunction[L, R], L, R](disjunction: D[L, R])(implicit F: Foldable[({ type E[X] = D[L, X] })# E]) =
    *     new FoldableOps[({ type E[X] = D[L, X] })# E, R](disjunction)
    * }}}
    */
  implicit def toFoldableOps[D[_, _] <: Disjunction[_, _], L, R](disjunction: D[L, R])(implicit Foldable: Foldable[D[L, *]]): FoldableOps[D[L, *], R] =
    new FoldableOps[D[L, *], R](disjunction)
}