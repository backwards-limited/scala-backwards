package com.backwards.typelevel.automatic_type_class_derivation.part1

import shapeless.{::, Generic, HList, HNil, Lazy}
import org.scalatest.matchers.must.Matchers
import org.scalatest.wordspec.AnyWordSpec

/**
 * [[https://medium.com/@mattroberts297/automatic-type-class-derivation-with-shapeless-part-one-a1251c5f9e16 Automatic Type Class Derivation with Shapeless — Part One]]
 *
 * Parse command line args into an ADT - Simple first attempt:
 * {{{
 *  my-app foo 1 true
 * }}}
 * parsed into
 * {{{
 *  SimpleArguments(alpha = "foo", beta = 1, charlie = true)
 * }}}
 *
 * Lazy stops the compiler giving up it's search for implicits too early.
 * The Scala compiler uses heuristics to make sure that it doesn't get stuck in an infinite recursion when resolving implicits.
 * For more complex data types, these heuristics tend to be too aggressive - The use of Lazy lets us workaround this issue.
 */
class Part1Spec extends AnyWordSpec with Matchers {
  "Parser" should {
    "apply via implicit recursion" in {
      val result = Parser[SimpleArguments].parse(List("foo", "1", "true"))

      pprint.pprintln(result)
      result mustBe SimpleArguments(alpha = "foo", beta = 1, charlie = true)
    }

    "apply explicitly" in {
      import Parser._

      val parser: Parser[SimpleArguments] =
        Parser[SimpleArguments](
          Lazy(
            genericParser(
              Generic[SimpleArguments],
              Lazy(
                hlistParser(
                  Lazy(stringParser),
                  hlistParser(
                    Lazy(intParser),
                    hlistParser(
                      Lazy(boolParser),
                      hnilParser
                    )
                  )
                )
              )
            )
          )
        )

      val result = parser.parse(List("foo", "1", "true"))

      pprint.pprintln(result)
      result mustBe SimpleArguments(alpha = "foo", beta = 1, charlie = true)
    }
  }
}

final case class SimpleArguments(alpha: String, beta: Int, charlie: Boolean)

trait Parser[A] {
  def parse(args: List[String]): A
}

/**
 * The apply method causes the Scala compiler to search for an implicit Parser[A].
 * It finds the genericParser method and that in turn causes it to look for an implicit Generic.Aux[A, R] and Parser[R].
 * Where R is a HList that Generic can create an A from.
 * It finds a Generic.Aux[A, R] thanks to the shapeless.Generic import (and a macro in the shapeless library).
 * The Parser[R] requirement, as you may have guessed, is satisfied by the hlist Parser method.
 * The implicit Parser[H] is satisfied by the, rather mundane, implicit values that handle primitive types and the Parser[T] is handled recursively until the terminal HNil case is reached.
 */
object Parser {
  def create[A](f: List[String] => A): Parser[A] =
    args => f(args)

  def apply[A](implicit st: Lazy[Parser[A]]): Parser[A] =
    st.value

  implicit def genericParser[A, R <: HList](
    implicit generic: Generic.Aux[A, R],
    parser: Lazy[Parser[R]]
  ): Parser[A] =
    create(args => generic.from(parser.value.parse(args)))

  implicit def hlistParser[H, T <: HList](
    implicit hParser: Lazy[Parser[H]],
    tParser: Parser[T]
  ): Parser[H :: T] =
    create(args => hParser.value.parse(args) :: tParser.parse(args.tail))

  implicit val hnilParser: Parser[HNil] =
    create(_ => HNil)

  implicit val stringParser: Parser[String] =
    create(_.head)

  implicit val intParser: Parser[Int] =
    create(_.head.toInt)

  implicit val boolParser: Parser[Boolean] =
    create(_.head.toBoolean)
}