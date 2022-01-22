package tech.backwards.typelevel.transform_adt

import shapeless._
import shapeless.tag._
import shapeless.labelled.FieldType
import shapeless.labelled._
import org.scalatest.matchers.must.Matchers
import org.scalatest.wordspec.AnyWordSpec

/**
 * [[http://www.beyondthelines.net/programming/fluent-a-deep-dive-into-shapeless-and-implicit-resolution/ Fluent – A deep dive into Shapeless and implicit resolution]]
 */
class TransformAdtToAdtSpec extends AnyWordSpec with Matchers {
  "Shapeless" should {
    "provides us with Singleton types for Int pre 2.13" in {
      /**
       * Think of 1 as a subtype of Int
       */
      def onlyOne(one: Witness.`1`.T): Witness.`1`.T = one

      onlyOne(1)
      "onlyOne(2)" mustNot compile
    }

    "provides us with Singleton types for Int" in {
      /**
       * Think of 1 as a subtype of Int
       */
      def onlyOne(one: 1): 1 = one

      onlyOne(1)
      "onlyOne(2)" mustNot compile
    }

    "provides us with Singleton types for String pre 2.13" in {
      /**
       * Think of "Hello" as a subtype of String
       */
      def onlyHello(hello: Witness.`"Hello"`.T): Witness.`"Hello"`.T = hello

      onlyHello("Hello")
      """onlyHello("Hi")""" mustNot compile
    }

    "provides us with Singleton types for String" in {
      /**
       * Think of "Hello" as a subtype of String
       */
      def onlyHello(hello: "Hello"): "Hello" = hello

      onlyHello("Hello")
      """onlyHello("Hi")""" mustNot compile
    }
  }

  """So we can use singleton types to represent our field names at the type level.
    |What’s even better is that Shapeless does it for us if we use LabelledGeneric instead of Generic.""".stripMargin should {
    final case class Circle(x: Double, y: Double, radius: Double, colour: String)

    val circle: Circle =
      Circle(1.0, 2.0, 3.0, "Blue")

    "Generic" in {
      val repr = Generic[Circle].to(circle)
      println(repr)
      // 1.0 :: 2.0 :: 3.0 :: Blue :: HNil

      val reprSpecific: Double :: Double :: Double :: String :: HNil = Generic[Circle].to(circle)
      println(reprSpecific)
      // 1.0 :: 2.0 :: 3.0 :: Blue :: HNil
    }

    "LabelledGeneric" in {
      val labelledRepr = LabelledGeneric[Circle].to(circle)
      println(labelledRepr)
      // 1.0 :: 2.0 :: 3.0 :: Blue :: HNil

      // WAIT - I thought LabelledGeneric would have the fields types, but this looks exactly the same as Generic.

      // However, check out the type.
      // Basically it’s still a HList but with refined types - Each refined type is called a Record (where type FieldType[K, +V] = V with KeyTag[K, V]).
      val labelledReprSpecific/*: FieldType[Double with KeyTag[Symbol with Tagged[{ type x }], Double], Double] ::
                                FieldType[Double with KeyTag[Symbol with Tagged[{ type y }], Double], Double] ::
                                FieldType[Double with KeyTag[Symbol with Tagged[{ type radius }], Double], Double] ::
                                FieldType[String with KeyTag[Symbol with Tagged[{ type colour }], String], String] :: HNil*/ =
        LabelledGeneric[Circle].to(circle)
    }
  }
}