package tech.backwards.fp.applicative

import tech.backwards.fp.functor.Functor
import tech.backwards.fp.monoid.Monoid
import tech.backwards.fp.writer.Writer

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
