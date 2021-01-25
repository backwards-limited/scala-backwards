package com.backwards.typelevel.automatic_type_class_derivation.part2

import shapeless.labelled.{FieldType, field}
import shapeless.{::, HList, HNil, LabelledGeneric, Lazy, Witness}
import org.scalatest.matchers.must.Matchers
import org.scalatest.wordspec.AnyWordSpec

/**
 * [[https://medium.com/@mattroberts297/automatic-type-class-derivation-with-shapeless-part-two-a7f6627f36fb Automatic Type Class Derivation with Shapeless — Part Two]]
 *
 * Use LabelledGeneric to retrieve case class field names at compile time.
 *
 * Parse command line args into an ADT - Second attempt:
 * {{{
 *  my-app --alpha foo --beta 1 --charlie
 * }}}
 * parsed into
 * {{{
 *  SimpleArguments(alpha = "foo", beta = 1, charlie = true)
 * }}}
 */
class Part2Spec extends AnyWordSpec with Matchers {
  "Parser" should {
    "apply via implicit recursion" in {
      val result = Parser[SimpleArguments].parse(List("--alpha", "a", "--beta", "1", "--charlie"))

      pprint.pprintln(result)
      result mustBe SimpleArguments(alpha = "a", beta = 1, charlie = true)
    }

    /*"apply explicitly" in {
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
    }*/
  }
}

final case class SimpleArguments(alpha: String, beta: Int, charlie: Boolean)

trait Parser[A] {
  def parse(args: List[String]): A
}

object Parser {
  def create[A](f: List[String] => A): Parser[A] =
    args => f(args)

  def apply[A](implicit st: Lazy[Parser[A]]): Parser[A] =
    st.value

  implicit def genericParser[A, R <: HList](
    implicit generic: LabelledGeneric.Aux[A, R],
    parser: Lazy[Parser[R]]
  ): Parser[A] =
    create(args => generic.from(parser.value.parse(args)))

  implicit def hlistParser[S <: Symbol, H, T <: HList](
    implicit hParser: Lazy[Parser[FieldType[S, H]]],
    tParser: Parser[T]
  ): Parser[FieldType[S, H] :: T] =
    create { args =>
      val hv: FieldType[S, H] =
        hParser.value.parse(args)

      val tv: T =
        tParser.parse(args)

      hv :: tv
    }

  /**
   * Witness is used to obtain the field name.
   * The field method lets us return a FieldType[S, String] instead of a String.
   */
  implicit def stringParser[S <: Symbol](implicit witness: Witness.Aux[S]): Parser[FieldType[S, String]] =
    create { args =>
      val name: String =
        witness.value.name

      val arg: String =
        args.dropWhile(_ != s"--$name").tail.head

      field[S](arg)
    }

  /**
   * Witness is used to obtain the field name.
   * The field method lets us return a FieldType[S, String] instead of a String.
   */
  implicit def intParser[S <: Symbol](implicit witness: Witness.Aux[S]): Parser[FieldType[S, Int]] =
    create { args =>
      val name: String =
        witness.value.name

      val arg: Int =
        args.dropWhile(_ != s"--$name").tail.head.toInt

      field[S](arg)
    }

  /**
   * Witness is used to obtain the field name.
   * The field method lets us return a FieldType[S, String] instead of a String.
   */
  implicit def booleanParser[S <: Symbol](implicit witness: Witness.Aux[S]): Parser[FieldType[S, Boolean]] =
    create { args =>
      val name: String =
        witness.value.name

      val arg: Boolean =
        args.contains(s"--$name")

      field[S](arg)
    }

  implicit val hnilParser: Parser[HNil] =
    create(_ => HNil)
}