package com.backwards.fp.monoid

object ListMonoidOps {
  implicit def listMonoid[T]: Monoid[List[T]] = new Monoid[List[T]] {
    def mzero: List[T] = Nil

    def mappend(lhs: List[T], rhs: List[T]): List[T] =
      lhs ++ rhs
  }
}