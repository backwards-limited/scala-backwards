package com.backwards.fp.monoid

import scala.language.implicitConversions

trait Monoid[T] {
  def mzero: T

  def mappend(lhs: T, rhs: T): T
}

object Monoid {
  def mzero[T: Monoid]: T = implicitly[Monoid[T]].mzero
}

class MonoidOps[T: Monoid](lhs: T) {
  def |+|(rhs: T): T = implicitly[Monoid[T]].mappend(lhs, rhs)
}

object MonoidOps {
  implicit def toMonoidOps[T: Monoid](t: T): MonoidOps[T] = new MonoidOps[T](t)
}