package com.backwards.maths

sealed trait Expr

class Op(val apply: (Double, Double) => Double) extends Expr

object Op {
  def unapply(op: Op): Option[(Double, Double) => Double] =
    Option(op.apply)
}

case object Exponent extends Op(math.pow)

case object Multiply extends Op(_ * _)

case object Divide extends Op(_ / _)

case object Add extends Op(_ + _)

case object Subtract extends Op(_ - _)

final case class Num(value: Double) extends Expr

final case class Exprs(value: List[Expr] = Nil) extends Expr {
  def +(expr: Expr): Exprs =
    Exprs(value :+ expr)
}