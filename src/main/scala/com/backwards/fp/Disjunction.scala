package com.backwards.fp

sealed abstract class Disjunction[+L, +R]

final case class LeftDisjunction[L, R](value: L) extends Disjunction[L, R]

final case class RightDisjunction[L, R](value: R) extends Disjunction[L, R]