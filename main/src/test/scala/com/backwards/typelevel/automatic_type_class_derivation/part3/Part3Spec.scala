package com.backwards.typelevel.automatic_type_class_derivation.part3

import shapeless.{::, Generic, HList, HNil, Lazy}
import org.scalatest.matchers.must.Matchers
import org.scalatest.wordspec.AnyWordSpec

/**
 * [[https://medium.com/@mattroberts297/automatic-type-class-derivation-with-shapeless-part-three-357709122e8b Automatic Type Class Derivation with Shapeless — Part Three]]
 *
 * Use Default to retrieve case class default values at compile time.
 */
class Part3Spec extends AnyWordSpec with Matchers {
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