package com.backwards

import scala.annotation.tailrec
import scala.collection.mutable
import scala.util.matching.Regex

object CSpec extends App {

  sealed trait Expr

  object Expr {
    val decimal: Regex = """[\d.]""".r

    val isDecimal: Char => Boolean =
      c => decimal.matches(String.valueOf(c))

    def parse(s: String): Exprs = {
      @tailrec
      def parse(s: List[Char], exprs: Exprs, stack: List[Exprs]): Exprs = s match {
        case '(' :: rest =>
          parse(rest, exprs, Exprs() :: stack)

        case ')' :: rest =>
          val subExprs :: nextStack = stack

          if (nextStack.isEmpty) {
            parse(rest, exprs + subExprs, nextStack)
          } else {
            val preSubExprs :: nextNextStack = nextStack

            parse(rest, exprs, preSubExprs + subExprs :: nextNextStack)
          }

        case '^' :: rest =>
          parsed(Exponent, rest, exprs, stack)

        case '*' :: rest =>
          parsed(Multiply, rest, exprs, stack)

        case '/' :: rest =>
          parsed(Divide, rest, exprs, stack)

        case '+' :: rest =>
          parsed(Add, rest, exprs, stack)

        case '-' :: rest =>
          val (numbers, remaining) = rest.span(isDecimal)

          if (numbers.isEmpty) {
            parsed(Subtract, remaining, exprs, stack)
          } else {
            parsed(Num(('-' +: numbers).mkString.toDouble), remaining, exprs, stack)
          }

        case c :: rest =>
          val (numbers, remaining) = rest.span(isDecimal)

          if (numbers.isEmpty) {
            parse(rest, exprs, stack)
          } else {
            parsed(Num((c +: numbers).mkString.toDouble), remaining, exprs, stack)
          }

        case Nil =>
          exprs
      }

      def parsed(expr: Expr, s: List[Char], exprs: Exprs, stack: List[Exprs]): Exprs =
        if (stack.isEmpty) {
          parse(s, exprs + expr, stack)
        } else {
          val subExprs :: nextStack = stack

          parse(s, exprs, subExprs + expr :: nextStack)
        }

      parse(s.toList, Exprs(), Nil)
    }

    def eval(expr: Expr): Double =
      eval(Exprs(List(expr)))

    def eval(exprs: List[Expr]): Double =
      eval(Exprs(exprs))

    def eval(exprs: Exprs): Double = {
      val narrow: Exprs => List[Expr] =
        _.value.map {
          case Exprs(ex) => Num(eval(ex))
          case ex => ex
        }

      difference(factor(exponent(narrow(exprs)))) match {
        case List(Num(v)) => v
        case _ => 0.0
      }
    }

    def exponent(exprs: List[Expr]): List[Expr] =
      op(exprs, Exponent)

    def factor(exprs: List[Expr]): List[Expr] =
      op(exprs, Multiply, Divide)

    def difference(exprs: List[Expr]): List[Expr] =
      op(exprs, Add, Subtract)

    def op(exprs: List[Expr], ops: Op*): List[Expr] = {
      @tailrec
      def go(exprs: List[Expr], results: List[Expr]): List[Expr] = exprs match {
        case Num(n1) :: (op @ Op(fn)) :: Num(n2) :: rest if ops.contains(op) => go(List(Num(fn(n1, n2))) ::: rest, results)
        case List(expr1, expr2, _*) => go(exprs.drop(2), results ::: List(expr1, expr2))
        case rest => results ::: rest
      }

      go(exprs, Nil)
    }
  }

  class Op(val apply: (Double, Double) => Double) extends Expr

  object Op {
    def unapply(op: Op): Option[(Double, Double) => Double] =
      Option(op.apply)
  }

  final case object Exponent extends Op(math.pow)

  final case object Multiply extends Op(_ * _)

  final case object Divide extends Op(_ / _)

  final case object Add extends Op(_ + _)

  final case object Subtract extends Op(_ - _)

  final case class Num(value: Double) extends Expr

  final case class Exprs(value: List[Expr] = Nil) extends Expr {
    def +(expr: Expr): Exprs =
      Exprs(value :+ expr)
  }

  import Expr._

  println(eval(Num(4)))
  println(eval(Exprs(List(Num(4)))))
  println(eval(Nil))
  println(eval(List(Num(4), Add, Num(2))))
  println(eval(List(Num(5), Add, Num(1), Add, Num(4), Add, Num(2))))
  println(eval(List(Num(5), Subtract, Num(1), Add, Num(4), Add, Num(2))))

  println(eval(List(Num(5), Add, Exprs(List(Num(5), Subtract, Num(1))))))
  println(eval(List(Exprs(List(Num(5), Subtract, Num(1))), Add, Num(5))))
  //println(Expr.eval(List(Num(5), Subtract, Num(1), Add, Exprs(List(Num(5), Subtract, Num(1))), Add, Num(2))))

  println(eval(List(Exprs(List(Num(5), Subtract, Exprs(List(Num(1), Add, Num(1))))), Add, Num(5))))

  println(eval(List(Num(4), Multiply, Num(6))))

  println("---------------------------------------------")

  //println(blah("4"))
  //println(blah("4 + 2"))
  //println(blah("4 + (6 + 5) + 2")) // List(Num(4), Add, Exprs(List(Num(6), Add, Num(4))), Add, Num(2))
  //println(parse("4 + (6 + 5) + 2")) // List(Num(4), Add, Exprs(List(Num(6), Add, Num(4))), Add, Num(2))

  println(parse("4 + (6 + -5) + 2"))
  println(parse("4 + (6 + 55) + 2"))


  println(parse("(2.16 - 48.34)^-1"))
}