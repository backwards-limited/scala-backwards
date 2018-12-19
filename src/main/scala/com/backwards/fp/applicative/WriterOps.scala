package com.backwards.fp.applicative

import com.backwards.fp.Writer
import com.backwards.fp.functor.Functor
import com.backwards.fp.monoid.Monoid

object WriterOps {
  /**
    * Because of using the "kind projector" compiler plugin the following becomes much easier:
    * {{{
    *   implicit def writerApplicative[W] = new Applicative[({ type E[X] = Writer[W, X] })# E]
    * }}}
    */
  implicit def writerApplicative[W](implicit F: Functor[Writer[W, ?]]): Applicative[Writer[W, ?]] = new Applicative[Writer[W, ?]] {
    def pure[A](a: A): Writer[W, A] = ???

    def <*>[A, R](ff: Writer[W, A => R])(fa: Writer[W, A]): Writer[W, R] = ???
  }

  /**
    * Because of using the "kind projector" compiler plugin the following becomes much easier:
    * {{{
    *   new ApplicativeOps[({ type E[X] = Writer[W, X] })# E, A, R](writer)
    * }}}
    */
  implicit def toApplicativeOps[W: Monoid, A, R](writer: Writer[W, A => R]) =
    new ApplicativeOps[Writer[W, ?], A, R](writer)
}