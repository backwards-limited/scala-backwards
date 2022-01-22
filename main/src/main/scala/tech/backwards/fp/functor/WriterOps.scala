package tech.backwards.fp.functor

import scala.language.implicitConversions
import tech.backwards.fp.monoid.Monoid
import tech.backwards.fp.writer.Writer

object WriterOps extends ToFunctorOps {
  /**
    * Because of using the "kind projector" compiler plugin the following becomes much easier:
    * {{{
    *   implicit def writerFunctor[W] = new Functor[({ type E[X] = Writer[W, X] })# E]
    * }}}
    */
  implicit def writerFunctor[W: Monoid]: Functor[Writer[W, *]] =
    new Functor[Writer[W, *]] {
      override def fmap[A, B](writer: Writer[W, A])(f: A => B): Writer[W, B] = {
        val (w, a) = writer.run()
        Writer(() => (w, f(a)))
      }
    }

  /**
    * Because of using the "kind projector" compiler plugin the following becomes much easier (which was only a starting point):
    * {{{
    *   implicit def toFunctorOps[W, T](writer: Writer[W, T]) = new FunctorOps[({ type E[X] = Writer[W, X] })# E, T](writer)
    * }}}
    */
  implicit def toFunctorOps[W, T, WRITER[_, _] <: Writer[W, T]](
    writer: WRITER[W, T]
  )(implicit Functor: Functor[WRITER[W, *]]): FunctorOps[WRITER[W, *], T] =
    new FunctorOps[WRITER[W, *], T](writer)

  implicit def toFunctionFunctorOps[A, B](f: A => B): WriterFunctorOps[A, B] =
    new WriterFunctorOps[A, B](f)
}

class WriterFunctorOps[A, B](f: A => B) {
  import WriterOps._

  def `<$>`[W: Monoid](writer: Writer[W, A]): Writer[W, B] =
    writerFunctor.fmap(writer)(f)
}