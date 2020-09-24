package com.backwards.maths

import scala.annotation.tailrec
import scala.math.BigDecimal
import scala.util.matching.Regex
import com.backwards.fp.Function.syntax._

/**
 * A simple calculator.
 * The following operations are supported (in order of precedence):
 * <pre>
 * 1 ()       Brackets
 * 2 -        Unary minus
 * 3 `^`        Exponent
 * 4 *, /     Multiply, Divide (left-to-right precedence)
 * 5 +, -     Add, Subtract (left-to-right precedence)
 * </pre>
 * A given String should result in a floating point number to the 5th digit after the dot.
 * e.g 14.132646 gets 14.13265, 14.132644 gets 14.13264, 14.132645 gets 14.13265.
 * <pre>
 * Input:                   Output:
 * 3 `^` 6 / 117            6.23077
 *
 * Input:                   Output:
 * (2.16 - 48.34) `^` -1    -0.02165
 * </pre>
 */
object Eval {
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