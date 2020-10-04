package com.backwards.fp.functor

import com.backwards.fp.{Disjunction, LeftDisjunction, RightDisjunction}
import com.backwards.fp.functor.Backwards._

object DisjunctionOps {
  implicit def leftDisjunctionFunctor[L, R]: Functor[LeftDisjunction[L, *]] = new Functor[LeftDisjunction[L, *]] {
    def fmap[A, B](fa: LeftDisjunction[L, A])(f: A => B): LeftDisjunction[L, B] = LeftDisjunction[L, B](fa.value)
  }

  implicit def rightDisjunctionFunctor[L, R]: Functor[RightDisjunction[L, *]] = new Functor[RightDisjunction[L, *]] {
    def fmap[A, B](fa: RightDisjunction[L, A])(f: A => B): RightDisjunction[L, B] = RightDisjunction(f(fa.value))
  }

  /**
    * Because of using the "kind projector" compiler plugin the following becomes much easier:
    * {{{
    *   implicit def disjunctionFunctor[L] = new Functor[({ type E[A] = Disjunction[L, A] })# E] {
    *     override def fmap[A, B](a: Disjunction[L, A])(f: A => B): Disjunction[L, B] = ???
    *   }
    * }}}
    */
  implicit def disjunctionFunctor[L]: Functor[Disjunction[L, *]] = new Functor[Disjunction[L, *]] {
    override def fmap[A, B](d: Disjunction[L, A])(f: A => B): Disjunction[L, B] = d match {
      case l @ LeftDisjunction(_) => leftDisjunctionFunctor.fmap(l)(f)
      case r @ RightDisjunction(_) => rightDisjunctionFunctor.fmap(r)(f)
    }
  }

  /**
    * Because of using the "kind projector" compiler plugin the following becomes much easier:
    * {{{
    *   implicit def toFunctorOps[D[L, R] <: Disjunction[L, R], L, R](disjunction: D[L, R])
    *                                                                (implicit F: Functor[({ type E[A] = D[L, A] })# E]) =
    *     new FunctorOps[({ type E[A] = D[L, A] })# E, R](disjunction)
    * }}}
    */
  implicit def toFunctorOps[D[_, _] <: Disjunction[_, _], L, R](disjunction: D[L, R])(implicit Functor: Functor[D[L, *]]): FunctorOps[D[L, *], R] =
    new FunctorOps[D[L, *], R](disjunction)
}