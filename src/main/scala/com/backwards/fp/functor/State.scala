package com.backwards.fp.functor

import scala.language.{higherKinds, implicitConversions}
import com.backwards.fp.State

object StateOps {
  /**
    * Because of using the "kind projector" compiler plugin the following becomes much easier:
    * {{{
    *   implicit def toFunctorOps[S, A, STATE[S, A]](state: STATE[S, A])(implicit functor: Functor[({ type E[X] = STATE[S, X] })# E]) =
    *     new FunctorOps[A, ({ type E[X] = STATE[S, X] })# E](state)
    * }}}
    */
  implicit def toFunctorOps[S, A, STATE[S, A]](state: STATE[S, A])(implicit functor: Functor[STATE[S, ?]]) =
    new FunctorOps[A, STATE[S, ?]](state)

  /**
    * Because of using the "kind projector" compiler plugin the following becomes much easier:
    * {{{
    *   implicit def stateFunctor[S] = new Functor[({ type E[X] = State[S, X] })# E] {
    *     override def fmap[A, B](state: State[S, A])(f: A => B): State[S, B] =
    *       State { s =>
    *         val (nextS, a) = state.run(s)
    *         (nextS, f(a))
    *       }
    * }}}
    */
  implicit def stateFunctor[S] = new Functor[State[S, ?]] {
    override def fmap[A, B](state: State[S, A])(f: A => B): State[S, B] =
      State { s =>
        val (nextS, a) = state.run(s)
        (nextS, f(a))
      }
  }

  implicit def toStateFunctionOps[A, B](f: A => B) = new StateFunctionOps(f)
}

class StateFunctionOps[A, B](f: A => B) {
  import com.backwards.fp.functor.StateOps._

  def `<$>`[S](state: State[S, A]): State[S, B] = state fmap f
}