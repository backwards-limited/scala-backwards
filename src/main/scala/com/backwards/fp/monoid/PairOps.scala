package com.backwards.fp.monoid

import com.backwards.fp.Pair
import com.backwards.fp.monoid.Backwards._

object PairOps {
  import MonoidOps._

  implicit def pairMonoid[A, B](implicit MonoidA: Monoid[A], MonoidB: Monoid[B]): Monoid[Pair[A, B]] = new Monoid[Pair[A, B]] {
    def mzero: Pair[A, B] =
      Pair(MonoidA.mzero, MonoidB.mzero)

    def mappend(lhs: Pair[A, B], rhs: Pair[A, B]): Pair[A, B] =
      Pair(lhs.a |+| rhs.a, lhs.b |+| rhs.b)
  }
}