package com.backwards.fp.functor

import scala.language.{higherKinds, implicitConversions}

final case class State[S, A](run: S => (S, A)) {
  def exec(s: S): S = run(s)._1

  def eval(s: S): A = run(s)._2
}

object State {
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

  def put[S](s: S): State[S, Unit] = State(_ => (s, ()))

  def get[S]: State[S, S] = State(s => (s, s))

  def modify[S](f: S => S): State[S, Unit] = State(s => (f(s), ()))
}

class StateFunctionOps[A, B](f: A => B) {
  def `<$>`[S](state: State[S, A]): State[S, B] = state fmap f
}