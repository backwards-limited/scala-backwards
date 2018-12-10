package com.backwards.fp.monad

import scala.language.implicitConversions
import com.backwards.fp.State

object StateOps {
  import com.backwards.fp.functor.StateOps._

  /**
    * Because of using the "kind projector" compiler plugin the following becomes much easier:
    * {{{
    *   implicit def stateMonad[S] = new Monad[({ type E[X] = State[S, X] })# E]
    * }}}
    */
  implicit def stateMonad[S]: Monad[State[S, ?]] = new Monad[State[S, ?]] {
    def pure[A](a: A): State[S, A] = State(s => (s, a))

    def flatMap[A, B](m: State[S, A])(f: A => State[S, B]): State[S, B] = {
      State { s =>
        val (nextS, a) = m.run(s)
        f(a).run(nextS)
      }
    }
  }

  /**
    * Because of using the "kind projector" compiler plugin the following becomes much easier:
    * {{{
    *   implicit def toMonadOps[S, A](state: State[S, A])(implicit M: Monad[({ type E[X] = State[S, X] })# E]) =
    *     new MonadOps[({ type E[X] = State[S, X] })# E, A](state)
    * }}}
    */
  implicit def toMonadOps[S, A](state: State[S, A])(implicit M: Monad[State[S, ?]]): MonadOps[State[S, ?], A] =
    new MonadOps[State[S, ?], A](state)

  implicit def toMonadPureOps[A](a: A): MonadPureOps[A] = new MonadPureOps(a)
}