package com.backwards.fp.monoid

import com.backwards.fp.Pair

object PairOps {
  import MonoidOps._

  implicit def pairMonoid[A, B](implicit MA: Monoid[A], MB: Monoid[B]): Monoid[Pair[A, B]] = new Monoid[Pair[A, B]] {
    def mzero: Pair[A, B] =
      Pair(MA.mzero, MB.mzero)

    def mappend(lhs: Pair[A, B], rhs: Pair[A, B]): Pair[A, B] =
      Pair(lhs.a |+| rhs.a, lhs.b |+| rhs.b)
  }
}