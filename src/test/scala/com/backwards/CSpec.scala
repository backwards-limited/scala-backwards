package com.backwards

import scala.annotation.tailrec
import scala.collection.mutable
import scala.util.matching.Regex
import scala.math.BigDecimal

/*
The goal of this challenge is to create a simple calculator.
The following operations should be supported (in order of precedence):
1   ()       Brackets
2   -        Unary minus
3   ^        Exponent
4   *, /     Multiply, Divide (left-to-right precedence)
5   +, -     Add, Subtract (left-to-right precedence)

Input:
Your program should read lines from standard input. Each line contains mathematical expression.
Each number in input expression is greater than -20,000 and less than 20,000. Each output number is greater than -20,000 and less than 20,000.

Output:
Print out the result of the calculation. If the output number is a floating point number it should be rounded to the 5th digit after the dot.
E.g 14.132646 gets 14.13265, 14.132644 gets 14.13264, 14.132645 gets 14.13265.

If the output number has less than 5 digits after the decimal point, you don't need to add zeros.
Note: don't use any kind of eval function.


Input:                  Output:
3^6 / 117               6.23077


Input:                  Output:
(2.16 - 48.34)^-1       -0.02165
*/
object CSpec extends App {
  object Pipe {
    import scala.util.chaining._

    implicit class PipeOps[A](val a: A) {
      implicit def |>[B](f: A => B): B = a pipe f
    }
  }

  import Pipe._

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
            parsed(Subtract, rest, exprs, stack)
          } else {
            parsed(Num(('-' +: numbers).mkString.toDouble), remaining, exprs, stack)
          }

        case all @ _ :: rest =>
          val (numbers, remaining) = all.span(isDecimal)

          if (numbers.isEmpty) {
            parse(rest, exprs, stack)
          } else {
            parsed(Num(numbers.mkString.toDouble), remaining, exprs, stack)
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

    def eval(s: String): BigDecimal =
      parse(s) |> eval |> BigDecimal.apply |> { _.setScale(5, BigDecimal.RoundingMode.HALF_UP) }

    def eval(expr: Expr): Double =
      eval(Exprs(List(expr)))

    def eval(exprs: List[Expr]): Double =
      eval(Exprs(exprs))

    def eval(exprs: Exprs): Double = {
      val narrow: Exprs => List[Expr] = _.value.map {
        case Exprs(value) => Num(eval(value))
        case ex => ex
      }

      exprs |> narrow |> exponent |> factor |> difference match {
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
        case Num(n1) :: (op @ Op(fn)) :: Num(n2) :: rest if ops.contains(op) =>
          go(List(Num(fn(n1, n2))) ::: rest, results)

        case List(expr1, expr2, _*) =>
          go(exprs.drop(2), results ::: List(expr1, expr2))

        case rest =>
          results ::: rest
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

  // 3^6 / 117               6.23077

  // Exprs(List(Num(3.0), Exponent, Num(6.0), Divide, Num(117)))
  println(eval(Exprs(List(Num(3.0), Exponent, Num(6.0), Divide, Num(117)))))

  // (2.16 - 48.34)^-1       -0.02165
  println(eval(Exprs(List(Exprs(List(Num(2.16), Subtract, Num(48.34))), Exponent, Num(-1.0)))))

  println("---------------------------------------------")

  BigDecimal(6.230769230769231).setScale(5, BigDecimal.RoundingMode.HALF_UP)

  6.230769230769231
  -0.021654395842355994



  //println(blah("4"))
  //println(blah("4 + 2"))
  //println(blah("4 + (6 + 5) + 2")) // List(Num(4), Add, Exprs(List(Num(6), Add, Num(4))), Add, Num(2))
  //println(parse("4 + (6 + 5) + 2")) // List(Num(4), Add, Exprs(List(Num(6), Add, Num(4))), Add, Num(2))

  println(parse("4 + (6 + -5) + 2"))
  println(parse("4 + (6 + 55) + 2"))


  println(parse("3^6 / 117"))
  println(parse("(2.16 - 48.34)^-1"))
}