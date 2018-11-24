package com.backwards.fp.monad

import com.backwards.fp.Writer
import com.backwards.fp.monoid.Monoid

object WriterOps {
  import com.backwards.fp.functor.WriterOps._

  /**
    * Because of using the "kind projector" compiler plugin the following becomes much easier:
    * {{{
    *   implicit def writerMonad[W] = new Monad[({ type E[A] = Writer[W, A] })# E]
    * }}}
    */
  implicit def writerMonad[W: Monoid] = new Monad[Writer[W, ?]] {
    def pure[A](a: A): Writer[W, A] = Writer(() => (Monoid.mzero, a))

    def flatMap[A, B](m: Writer[W, A])(f: A => Writer[W, B]): Writer[W, B] = {
      val (_, a) = m.run()
      f(a)
    }
  }
}