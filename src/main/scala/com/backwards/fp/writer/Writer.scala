package com.backwards.fp.writer

import com.backwards.fp.monoid.Backwards.Monoid

object Backwards {
  final case class Writer[W: Monoid, A](run: () => (W, A))

  object Writer {
    def tell[W: Monoid](w: W): Writer[W, Unit] =
      Writer(() => (w, ()))
  }
}