package com.backwards.fp.applicative

import scala.language.implicitConversions
import com.backwards.fp.functor.Backwards.Functor
import com.backwards.fp.monoid.Backwards.Monoid
import com.backwards.fp.applicative.Backwards._
import com.backwards.fp.writer.Backwards.Writer

object WriterOps {
  /**
    * Because of using the "kind projector" compiler plugin the following becomes much easier:
    * {{{
    *   implicit def writerApplicative[W] = new Applicative[({ type E[X] = Writer[W, X] })# E]
    * }}}
    */
  implicit def writerApplicative[W: Monoid](implicit Functor: Functor[Writer[W, *]]): Applicative[Writer[W, *]] = new Applicative[Writer[W, *]] {
    def pure[A](a: A): Writer[W, A] = Writer(() => (implicitly[Monoid[W]].mzero, a))

    def <*>[A, R](ff: Writer[W, A => R])(fa: Writer[W, A]): Writer[W, R] = {
      val (w, a) = fa.run()
      val (ww, f) = ff.run()
      Writer(() => (implicitly[Monoid[W]].mappend(ww, w), f(a)))
    }
  }

  /**
    * Because of using the "kind projector" compiler plugin the following becomes much easier:
    * {{{
    *   new ApplicativeOps[({ type E[X] = Writer[W, X] })# E, A, R](writer)
    * }}}
    */
  implicit def toApplicativeOps[W: Monoid, A, R](writer: Writer[W, A => R]): ApplicativeOps[Writer[W, *], A, R] =
    new ApplicativeOps[Writer[W, *], A, R](writer)
}