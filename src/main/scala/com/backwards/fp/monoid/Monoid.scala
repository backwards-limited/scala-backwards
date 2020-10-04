package com.backwards.fp.monoid

import scala.language.implicitConversions

object Backwards {
  trait Monoid[T] {
    def mzero: T

    def mappend(lhs: T, rhs: T): T
  }

  object Monoid {
    def apply[T: Monoid]: Monoid[T] = implicitly

    def mzero[T: Monoid]: T = Monoid[T].mzero
  }

  class MonoidOps[T: Monoid](lhs: T) {
    def |+|(rhs: T): T = Monoid[T].mappend(lhs, rhs)
  }

  object MonoidOps {
    implicit def toMonoidOps[T: Monoid](t: T): MonoidOps[T] = new MonoidOps[T](t)
  }
}