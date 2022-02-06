package tech.backwards.fp.monad

import tech.backwards.fp.{Disjunction, LeftDisjunction, RightDisjunction}
import tech.backwards.fp.RightDisjunction

object DisjunctionOps {
  import tech.backwards.fp.functor.DisjunctionOps._

  /**
   * Because of using the "kind projector" compiler plugin the following becomes much easier:
   * {{{
   *   implicit def disjunctionMonad[L] = new Monad[({ type E[A] = Disjunction[L, A] })# E]
   * }}}
   */
  implicit def disjunctionMonad[L]: Monad[Disjunction[L, *]] = new Monad[Disjunction[L, *]] {
    def pure[A](a: A): Disjunction[L, A] =
      RightDisjunction(a)

    def flatMap[A, B](m: Disjunction[L, A])(f: A => Disjunction[L, B]): Disjunction[L, B] =
      m match {
        case LeftDisjunction(value) => LeftDisjunction[L, B](value)
        case RightDisjunction(value) => f(value)
      }
  }

  implicit def toMonadOps[L, R, D[L, R] <: Disjunction[L, R]](
    disjunction: D[L, R]
  )(implicit Monad: Monad[D[L, *]]): MonadOps[D[L, *], R] =
    new MonadOps[D[L, *], R](disjunction)

  implicit def toMonadPureOps[A](a: A): MonadPureOps[A] =
    new MonadPureOps(a)
}