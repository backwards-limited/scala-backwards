package com.backwards.fp.monoid

import com.backwards.fp.Pair

object PairOps {
  import MonoidOps._

  implicit def pairMonoid[A, B](implicit aMonoid: Monoid[A], bMonoid: Monoid[B]): Monoid[Pair[A, B]] = new Monoid[Pair[A, B]] {
    def mzero: Pair[A, B] =
      Pair(aMonoid.mzero, bMonoid.mzero)

    def mappend(lhs: Pair[A, B], rhs: Pair[A, B]): Pair[A, B] =
      Pair(lhs.a |+| rhs.a, lhs.b |+| rhs.b)
  }
}