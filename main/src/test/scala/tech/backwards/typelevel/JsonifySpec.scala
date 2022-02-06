package tech.backwards.typelevel

import org.scalatest.matchers.must.Matchers
import org.scalatest.wordspec.AnyWordSpec

/**
 * [[https://blog.softwaremill.com/how-not-to-use-shapeless-for-cross-layer-conversions-in-scala-9ac36363aed9 How (not) to use shapeless for cross-layer conversions]]
 */
class JsonifySpec extends AnyWordSpec with Matchers {
  object ADT {
    case class Mead(name: String, honeyRatio: Double, agingYears: Double)
  }

  object DTO {
    case class Mead(name: String, honey_ratio: Double, aging_years: Double)
  }

  "Conversion" should {
    "be done semi-manually" in {
      import shapeless._
      import shapeless.syntax.std.product.productOps // for toRecord
      import record._   // for renameField

      def fromADT(adtMead: ADT.Mead): DTO.Mead = {
        val dGeneric = LabelledGeneric[ADT.Mead]
        val jGeneric = LabelledGeneric[DTO.Mead]

        val adtMeadRecord = adtMead.toRecord

        val d1 = adtMeadRecord.renameField(Symbol("honeyRatio"), Symbol("honey_ratio"))
        val d2 = d1.renameField(Symbol("agingYears"), Symbol("aging_years"))

        jGeneric.from(d2)
      }

      println("=" * 10 + " Semi")

      val adtMead = ADT.Mead("poltorak", 0.5, 10)
      pprint.pprintln(adtMead)

      val dtoMead = fromADT(adtMead)
      pprint.pprintln(dtoMead)
    }

    "more automatically" in {
      def string2SnakeCase(s: String): String =
        s.replaceAll(raw"(\p{Upper})", "_$1").toLowerCase()

      def symbol2SnakeCase(s: Symbol): Symbol =
        Symbol(string2SnakeCase(s.name))

      println("\n" + "Camel case to snake case example:")
      println(string2SnakeCase("scoobyDoo"))

      /*
      LabelledGeneric representation is HList-based, specifically it’s always an HList of FieldTypes, which is a simple alias:

      type FieldType[K, +V] = V with KeyTag[K, V]
      */

      // Now if the fields are of the same type and in the same order, then we just use Generic for complete automation:
      import shapeless.Generic

      println("\n" + "=" * 10 + " Auto with Generic")

      val adtMead = ADT.Mead("poltorak", 0.5, 10)
      pprint.pprintln(adtMead)

      val dtoMead = Generic[DTO.Mead].from(Generic[ADT.Mead].to(adtMead))
      pprint.pprintln(dtoMead)
    }

    "automatic" in {
      import shapeless.Generic
      import shapeless.HList

      trait SameReprConverter[From, To] {
        def apply(from: From): To
      }

      implicit def sameReprConverterFrom[
        Repr <: HList,
        From: Generic.Aux[*, Repr],
        To: Generic.Aux[*, Repr]
      ]: SameReprConverter[From, To] =
        (from: From) => Generic[To].from(Generic[From].to(from))

      def fromADT[ADT, DTO](adt: ADT)(implicit converter: SameReprConverter[ADT, DTO]): DTO =
        converter(adt)

      println("\n" + "=" * 10 + " Auto with Generic and HList")

      val adtMead = ADT.Mead("poltorak", 0.5, 10)
      pprint.pprintln(adtMead)

      val dtoMead = fromADT[ADT.Mead, DTO.Mead](adtMead)
      pprint.pprintln(dtoMead)
    }

    "improved automatic" in {
      import cats.implicits._ // for asLeft / asRight
      import shapeless.Generic
      import shapeless.HList
      import shapeless.LabelledGeneric
      import shapeless.ops._

      trait SameReprConverter[From, To] {
        def apply(from: From): To
      }

      implicit def sameReprConverterFrom[
        Repr <: HList,
        From: Generic.Aux[*, Repr],
        To: Generic.Aux[*, Repr]
      ]: SameReprConverter[From, To] =
        (from: From) => Generic[To].from(Generic[From].to(from))

      trait FieldNames[T] {
        def apply(): List[String]
      }

      implicit def keysOfType[T, FieldRepr <: HList, OutKeys <: HList](
        implicit lGeneric: LabelledGeneric.Aux[T, FieldRepr],
        keys: record.Keys.Aux[FieldRepr, OutKeys],
        tToTraversable: hlist.ToTraversable.Aux[OutKeys, List, Symbol]
      ): FieldNames[T] =
        () => keys().toList.map(_.name)

      def string2SnakeCase(s: String): String =
        s.replaceAll(raw"(\p{Upper})", "_$1").toLowerCase()

      def symbol2SnakeCase(s: Symbol): Symbol =
        Symbol(string2SnakeCase(s.name))

      def fromADT[ADT, DTO](adt: ADT)(
        implicit adtFieldNames: FieldNames[ADT],
        dtoFieldNames: FieldNames[DTO],
        converter: SameReprConverter[ADT, DTO]
      ): List[String] Either DTO =
        adtFieldNames().map(string2SnakeCase).diff(dtoFieldNames()) match {
          case Nil   => converter(adt).asRight
          case diffs => diffs.asLeft
        }

      println("\n" + "=" * 10 + " Auto with LabelledGeneric and HList")

      val adtMead = ADT.Mead("poltorak", 0.5, 10)
      pprint.pprintln(adtMead)

      val dtoMead = fromADT[ADT.Mead, DTO.Mead](adtMead)
      pprint.pprintln(dtoMead)
    }
  }
}