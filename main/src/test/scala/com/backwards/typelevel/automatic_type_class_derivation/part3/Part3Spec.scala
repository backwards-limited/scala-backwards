package com.backwards.typelevel.automatic_type_class_derivation.part3

import shapeless.{::, Generic, HList, HNil, Lazy}
import org.scalatest.matchers.must.Matchers
import org.scalatest.wordspec.AnyWordSpec
import shapeless.LabelledGeneric
import shapeless.{HList, HNil, ::}
import shapeless.Lazy
import shapeless.Witness
import shapeless.labelled.FieldType
import shapeless.labelled.field
import shapeless.Default

/**
 * [[https://medium.com/@mattroberts297/automatic-type-class-derivation-with-shapeless-part-three-357709122e8b Automatic Type Class Derivation with Shapeless — Part Three]]
 *
 * Use Default to retrieve case class default values at compile time.
 */
class Part3Spec extends AnyWordSpec with Matchers {
  "Parser" should {
    "apply via implicit recursion" in {
      val result = Parser[SimpleArguments].parse(List("--beta", "1", "--charlie"))

      pprint.pprintln(result)
      result mustBe SimpleArguments(beta = 1, charlie = true)
    }
  }
}

final case class SimpleArguments(alpha: String = "alpha", beta: Int, charlie: Boolean)

trait UnderlyingParser[A, B] {
  def parse(args: List[String], defaults: B): A
}

object UnderlyingParser {
  def create[A, B](f: (List[String], B) => A): UnderlyingParser[A, B] =
    (args, defaults) => f(args, defaults)

  implicit def hlistParser[S <: Symbol, H, T <: HList, TD <: HList](
    implicit witness: Witness.Aux[S],
    hParser: Lazy[UnderlyingParser[FieldType[S, H], Option[H]]],
    tParser: UnderlyingParser[T, TD]
  ): UnderlyingParser[FieldType[S, H] :: T, Option[H] :: TD] =
    create { (args, defaults) =>
      val hv: FieldType[S, H] =
        hParser.value.parse(args, defaults.head)

      val tv: T =
        tParser.parse(args, defaults.tail)

      hv :: tv
    }

  implicit def stringParser[S <: Symbol](
    implicit witness: Witness.Aux[S]
  ): UnderlyingParser[FieldType[S, String], Option[String]] =
    create { (args, defaultArg) =>
      val name: String =
        witness.value.name

      val providedArg: Option[String] =
        getArgFor(args, name)

      // TODO: Use Either instead of throwing an exception.
      val arg: String =
        providedArg orElse defaultArg getOrElse (throw new IllegalArgumentException(s"Missing argument $name"))

      field[S](arg)
    }

  implicit def intParser[S <: Symbol](
    implicit witness: Witness.Aux[S]
  ): UnderlyingParser[FieldType[S, Int], Option[Int]] =
    create { (args, defaultArg) =>
      val name: String =
        witness.value.name

      val providedArg: Option[Int] =
        getArgFor(args, name).map(_.toInt)

      // TODO: Use Either instead of throwing an exception.
      val arg: Int =
        providedArg orElse defaultArg getOrElse (throw new IllegalArgumentException(s"Missing argument $name"))

      field[S](arg)
    }

  implicit def booleanParser[S <: Symbol](
    implicit witness: Witness.Aux[S]
  ): UnderlyingParser[FieldType[S, Boolean], Option[Boolean]] =
    create { (args, default) =>
      val name: String =
        witness.value.name

      val arg: Boolean =
        if (args.contains(s"--$name")) true else default getOrElse false

      field[S](arg)
    }

  implicit val hnilParser: UnderlyingParser[HNil, HNil] =
    create { (_, _) => HNil }

  private def getArgFor(args: List[String], name: String): Option[String] = {
    val indexOfName: Int =
      args.indexOf(s"--$name")

    val indexAfterName: Int =
      indexOfName + 1

    if (indexOfName > -1 && args.isDefinedAt(indexAfterName)) {
      Some(args(indexAfterName))
    } else {
      None
    }
  }
}

trait Parser[A] {
  def parse(args: List[String]): A
}

object Parser {
  def apply[A](implicit st: Lazy[Parser[A]]): Parser[A] =
    st.value

  def create[A](f: List[String] => A): Parser[A] =
    args => f(args)

  implicit def genericParser[A, R <: HList, D <: HList](
    implicit defaults: Default.AsOptions.Aux[A, D],
    generic: LabelledGeneric.Aux[A, R],
    parser: Lazy[UnderlyingParser[R, D]]
  ): Parser[A] =
    create { args =>
      generic.from(parser.value.parse(args, defaults()))
    }
}