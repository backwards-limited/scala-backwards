package com.backwards.fp.applicative

import scala.language.implicitConversions
import com.backwards.fp.applicative.Backwards._
import com.backwards.fp.state.Backwards.State

object StateOps {
  import com.backwards.fp.functor.StateOps._

  /**
    * Because of using the "kind projector" compiler plugin the following becomes much easier:
    * {{{
    *   implicit def stateApplicative[S]: Applicative[({ type E[X] = State[S, X] })# E] = new Applicative[({ type E[X] = State[S, X] })# E]
    * }}}
    */
  implicit def stateApplicative[S]: Applicative[State[S, *]] = new Applicative[State[S, *]] {
    override def pure[A](a: A): State[S, A] = State(s => (s, a))

    override def <*>[A, R](ff: State[S, A => R])(state: State[S, A]): State[S, R] = {
      State { s1 =>
        val (s2, a) = state.run(s1)
        val (s3, f) = ff.run(s2)

        (s3, f(a))
      }
    }
  }

  /**
    * Because of using the "kind projector" compiler plugin the following becomes much easier:
    * {{{
    *   implicit def toApplicativeOps[S, A, R](state: State[S, A => R]): ApplicativeOps[({ type E[X] = State[S, X] })# E, A, R] =
    *     new ApplicativeOps[({ type E[X] = State[S, X] })# E, A, R](state)
    * }}}
    */
  implicit def toApplicativeOps[S, A, R](state: State[S, A => R]): ApplicativeOps[State[S, *], A, R] =
    new ApplicativeOps[State[S, *], A, R](state)

  implicit class StateOps[A](a: A) {
    def pure[S]: State[S, A] = stateApplicative[S].pure(a)
  }
}