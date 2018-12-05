package com.backwards.fp.monad

import com.backwards.fp.{Disjunction, LeftDisjunction, RightDisjunction}

object DisjunctionOps {
  import com.backwards.fp.functor.DisjunctionOps._

  /**
    * Because of using the "kind projector" compiler plugin the following becomes much easier:
    * {{{
    *   implicit def disjunctionMonad[L] = new Monad[({ type E[A] = Disjunction[L, A] })# E]
    * }}}
    */
  implicit def disjunctionMonad[L] = new Monad[Disjunction[L, ?]] {
    def pure[A](a: A): Disjunction[L, A] = RightDisjunction(a)

    def flatMap[A, B](m: Disjunction[L, A])(f: A => Disjunction[L, B]): Disjunction[L, B] = m match {
      case LeftDisjunction(value) => LeftDisjunction[L, B](value)
      case RightDisjunction(value) => f(value)
    }
  }

  implicit def toMonadOps[D[L, R] <: Disjunction[L, R], L, R](disjunction: D[L, R])(implicit M: Monad[D[L, ?]]) =
    new MonadOps[D[L, ?], R](disjunction)
}