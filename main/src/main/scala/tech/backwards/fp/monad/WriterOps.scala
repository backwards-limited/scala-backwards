package tech.backwards.fp.monad

import tech.backwards.fp.monoid.Monoid
import tech.backwards.fp.monoid.MonoidOps.toMonoidOps
import tech.backwards.fp.writer.Writer

object WriterOps {
  import tech.backwards.fp.functor.WriterOps._

  /**
   * Because of using the "kind projector" compiler plugin the following becomes much easier:
   * {{{
   *   implicit def writerMonad[W] = new Monad[({ type E[A] = Writer[W, A] })# E]
   * }}}
   */
  implicit def writerMonad[W: Monoid]: Monad[Writer[W, *]] = new Monad[Writer[W, *]] {
    def pure[A](a: A): Writer[W, A] =
      Writer(() => (Monoid.mzero, a))

    def flatMap[A, B](m: Writer[W, A])(f: A => Writer[W, B]): Writer[W, B] = {
      val (w1, a) = m.run()
      val (w2, b) = f(a).run()

      Writer(() => (w1 |+| w2, b))
    }
  }

  /**
   * Because of using the "kind projector" compiler plugin the following becomes much easier:
   * {{{
   *   implicit def toMonadOps[W: Monoid, A](writer: Writer[W, A]): MonadOps[({ type E[X] = Writer[W, X] })# E, A] =
   *     new MonadOps[({ type E[X] = Writer[W, X] })# E, A](writer)
   * }}}
   */
  implicit def toMonadOps[W: Monoid, A](writer: Writer[W, A]): MonadOps[Writer[W, *], A] =
    new MonadOps[Writer[W, *], A](writer)

  implicit def toMonadPureOps[A](a: A): MonadPureOps[A] =
    new MonadPureOps(a)
}