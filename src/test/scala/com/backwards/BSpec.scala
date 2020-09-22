package com.backwards

import scala.annotation.tailrec
import scala.collection.mutable

object BSpec extends App {

  sealed trait Expr

  object Expr {
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

      difference(factor(narrow(exprs))) match {
        case List(Num(v)) => v
        case _ => 0.0
      }
    }

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

  final case object Multiply extends Op(_ * _)

  final case object Divide extends Op(_ / _)

  final case object Add extends Op(_ + _)

  final case object Subtract extends Op(_ - _)

  final case class Num(value: Double) extends Expr

  final case class Exprs(value: List[Expr]) extends Expr

  println(Expr.eval(Num(4)))
  println(Expr.eval(Exprs(List(Num(4)))))
  println(Expr.eval(Nil))
  println(Expr.eval(List(Num(4), Add, Num(2))))
  println(Expr.eval(List(Num(5), Add, Num(1), Add, Num(4), Add, Num(2))))
  println(Expr.eval(List(Num(5), Subtract, Num(1), Add, Num(4), Add, Num(2))))

  println(Expr.eval(List(Num(5), Add, Exprs(List(Num(5), Subtract, Num(1))))))
  println(Expr.eval(List(Exprs(List(Num(5), Subtract, Num(1))), Add, Num(5))))
  //println(Expr.eval(List(Num(5), Subtract, Num(1), Add, Exprs(List(Num(5), Subtract, Num(1))), Add, Num(2))))

  println(Expr.eval(List(Exprs(List(Num(5), Subtract, Exprs(List(Num(1), Add, Num(1))))), Add, Num(5))))

  println(Expr.eval(List(Num(4), Multiply, Num(6))))

  println("---------------------------------------------")

  //println(blah("4"))
  //println(blah("4 + 2"))
  //println(blah("4 + (6 + 5) + 2")) // List(Num(4), Add, Exprs(List(Num(6), Add, Num(4))), Add, Num(2))
  //println(parse("4 + (6 + 5) + 2")) // List(Num(4), Add, Exprs(List(Num(6), Add, Num(4))), Add, Num(2))

  println(parse("4 + (6 + -5) + 2"))
  println(parse("4 + (6 + 55) + 2"))

  def parse(s: String): List[Expr] = {
    def parseOp(op: Op, s: List[Char], exprs: List[Expr], stack: mutable.Stack[List[Expr]]): List[Expr] = {
      val subExprs = stack.pop()
      stack.push(subExprs :+ op)
      parse(s, exprs, stack)
    }

    def parseNum(num: Num, s: List[Char], exprs: List[Expr], stack: mutable.Stack[List[Expr]]): List[Expr] = {
      val subExprs = stack.pop()
      stack.push(subExprs :+ num)
      parse(s, exprs, stack)
    }

    @tailrec
    def parse(s: List[Char], exprs: List[Expr], stack: mutable.Stack[List[Expr]]): List[Expr] = s match {
      case '(' :: rest =>
        parse(rest, exprs, stack.push(Nil))

      case ')' :: rest =>
        val subExprs = stack.pop()

        if (stack.isEmpty) {
          parse(rest, exprs :+ Exprs(subExprs), stack)
        } else {
          val preSubExprs = stack.pop()
          stack.push(preSubExprs :+ Exprs(subExprs))
          parse(rest, exprs, stack)
        }

      case c :: rest if c.isDigit =>
        val (numbers, remaining) = rest.span(_.isDigit)

        parseNum(Num((c +: numbers).mkString.toDouble), remaining, exprs, stack)

      case '*' :: rest =>
        parseOp(Multiply, rest, exprs, stack)

      case '/' :: rest =>
        parseOp(Divide, rest, exprs, stack)

      case '+' :: rest =>
        parseOp(Add, rest, exprs, stack)

      case '-' :: rest =>
        val (numbers, remaining) = rest.span(_.isDigit)

        if (numbers.isEmpty) {
          parseOp(Subtract, remaining, exprs, stack)
        } else {
          parseNum(Num(('-' +: numbers).mkString.toDouble), remaining, exprs, stack)
        }

      case _ :: rest =>
        parse(rest, exprs, stack)

      case Nil =>
        exprs
    }

    parse(s"($s)".toList, Nil, new mutable.Stack[List[Expr]])
  }
}