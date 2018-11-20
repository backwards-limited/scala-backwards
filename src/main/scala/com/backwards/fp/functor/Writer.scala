package com.backwards.fp.functor

import scala.language.{higherKinds, implicitConversions}

final case class Writer[W, A](run: () => (W, A))

object Writer {
  /**
    * Because of using the "kind projector" compiler plugin the following becomes much easier (which was only a starting point):
    * {{{
    *   implicit def toFunctorOps[A, W](a: Writer[W, A]) = new FunctorOps[A, ({ type E[X] = Writer[W, X] }) # E](a)
    * }}}
    */
  implicit def toFunctorOps[W, A, WRITER[W, A] <: Writer[W, A]](writer: WRITER[W, A])(implicit F: Functor[WRITER[W, ?]]) =
    new FunctorOps[A, WRITER[W, ?]](writer)

  /**
    * Because of using the "kind projector" compiler plugin the following becomes much easier:
    * {{{
    *   implicit def writerFunctor[W] = new Functor[({ type E[X] = Writer[W, X] })# E]
    * }}}
    */
  implicit def writerFunctor[W] = new Functor[Writer[W, ?]] {
    override def fmap[A, B](writer: Writer[W, A])(f: A => B): Writer[W, B] = {
      val (w, a) = writer.run()
      Writer(() => (w, f(a)))
    }
  }

  implicit def toFunctionFunctorOps[A, B](f: A => B) = new WriterFunctorOps[A, B](f)
}

class WriterFunctorOps[A, B](f: A => B) {
  def `<$>`[W](writer: Writer[W, A]): Writer[W, B] = writer fmap f
}