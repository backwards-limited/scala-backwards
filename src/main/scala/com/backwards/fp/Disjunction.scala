package com.backwards.fp

sealed abstract class Disjunction[+L, +R]

object Disjunction {
  def left[L, R](lv: L): Disjunction[L, R] = LeftDisjunction[L, R](lv)

  def right[L, R](rv: R): Disjunction[L, R] = RightDisjunction[L, R](rv)
}

final case class LeftDisjunction[L, R](value: L) extends Disjunction[L, R]

final case class RightDisjunction[L, R](value: R) extends Disjunction[L, R]