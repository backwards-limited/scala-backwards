package com.backwards.fp.monoid

import com.backwards.fp.monoid.Backwards.Monoid

object ListOps {
  implicit def listMonoid[T]: Monoid[List[T]] = new Monoid[List[T]] {
    def mzero: List[T] = Nil

    def mappend(lhs: List[T], rhs: List[T]): List[T] =
      lhs ++ rhs
  }
}