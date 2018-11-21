package com.backwards.fp.functor

import scala.language.{higherKinds, implicitConversions}
import com.backwards.fp.Writer

object WriterOps {
  /**
    * Because of using the "kind projector" compiler plugin the following becomes much easier (which was only a starting point):
    * {{{
    *   implicit def toFunctorOps[W, A](a: Writer[W, A]) = new FunctorOps[({ type E[X] = Writer[W, X] }) # E, A](a)
    * }}}
    */
  implicit def toFunctorOps[WRITER[W, A] <: Writer[W, A], W, A](writer: WRITER[W, A])(implicit F: Functor[WRITER[W, ?]]) =
    new FunctorOps[WRITER[W, ?], A](writer)

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
  import com.backwards.fp.functor.WriterOps._

  def `<$>`[W](writer: Writer[W, A]): Writer[W, B] = writer fmap f
}