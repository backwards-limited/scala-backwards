package tech.backwards.typelevel.learning1

import scala.collection.immutable
import org.scalatest.matchers.must.Matchers
import org.scalatest.wordspec.AnyWordSpec
import shapeless._
import shapeless.labelled.{FieldType, KeyTag}
import shapeless.syntax.SingletonOps
import shapeless.syntax.singleton._

/**
 * Type level programming
 * {{{
 *   App representation                            Generic representation                External representation
 *
 *   Product (case class)                          HList                                 JSON
 *     field A                                       A field A
 *     field B                                       B field B                           SQL Database
 *
 *                                                                                       Binary Codec
 *   Sum (sealed trait/class)                      Coproduct
 *     ^               ^                             class A
 *     |               |                             class B
 *     Concrete        Concrete
 *     Class A         Class B
 * }}}
 *
 * Typed (essentially static) programming
 * {{{
 *   ----------------------------------------------------------
 *                         Compile Time
 *
 *   Input Type                                    Output Type
 *       ^                                               ^
 *       |                                               |
 *    has type                                        has type
 *   ----|-----------------------------------------------|-----
 *       |                                               |
 *       |                                               |
 *   Input Data -----------> Function ------------> Output Data
 *
 *                           Run Time
 *   ----------------------------------------------------------
 * }}}
 *
 * vs Type level programming
 * {{{
 *   ----------------------------------------------------------
 *                         Compile Time
 *
 *   Input Type ------> Type-level function -----> Output Type
 *       ^                      ^                        ^
 *       |                      |                        |
 *    has type             has signature              has type
 *   ----|-----------------------------------------------|-----
 *       |                      |                        |
 *       |                      |                        |
 *   Input Data -----------> Function ------------> Output Data
 *
 *                           Run Time
 *   ----------------------------------------------------------
 * }}}
 *
 * We enable stronger typing, encouraging correctness as with regular static types.
 *   - Flowing type information through functions to accurately preserve type information about values as they are transformed by a program
 *   - Embedding computations into the type-system so that the result type of a function is computed in terms of the input types
 *   - Using very specific Singleton Types that describe a single value
 */
class L0Spec extends AnyWordSpec with Matchers {
  sealed trait Discovery

  case object TheoryOfRelativity extends Discovery

  case object Calculus extends Discovery

  "Type level programming" should {
    "introduce Shapeless" in {
      // Correctly does not compile
      // Some(1) mustEqual 1

      val scientist = "Einstein" :: 1879 :: TheoryOfRelativity :: HNil

      val name: String = scientist(0)
      name mustEqual "Einstein"

      val birthYear: Int = scientist(1)
      birthYear mustEqual 1879

      "val compilationError = scientist(5)" mustNot compile

      println(scientist)
    }

    "show that Scala collections uses type-level computation to vary its static return type depending on context" in {
      val m: Map[String, Int] =
        Map("Bob" -> 2)

      val anotherMap: Map[String, Int] =
        m map { case (k, v) => k -> v * 2 }

      val iterable: immutable.Iterable[Int] =
        m map { case (k, v) => v * 2 }
    }
  }

  /**
   * HLists are linked-list data structures that include type-information about all their elements.
   * The H stands for "heterogeneous", because the elements dont share a single "homogeneous" type.
   * HLists are like tuples in that all elements contribute to the overall type, but like lists in that they can grow, shrink, concat, fold etc.
   *
   * As HLists are constructed from typed values, a corresponding linked-list of types is constructed at the type-level.
   *
   * Shapeless provides many of the typical operations of a collections API over HLists, including take, drop, map, fold and indexing.
   * To map over an HList containing several element types, Shapeless introduces polymorphic functions (eg `Poly1`)
   * which, unlike regular mono-morphic functions, are defined for multiple input/output types.
   *
   * To perform transformations over HLists while preserving type information about all the elements requires type level computation.
   * This shows up in the signatures of shapeless library methods as implicit parameters that determine the output type.
   */
  "Shapeless HList" should {
    "" in {

    }
  }

  /**
   * Witness creates a so-called literal-based singleton type.
   * Literal type means it's a type that can only accept one value.
   */
  "Shapeless Witness" should {
    "" in {
      // If you create a function like this:
      def f12(x: Witness.`120`.T): Witness.`120`.T = x

      // it would accept only integer 120, but not 121.
      println(f12(120))
      "f12(121)" mustNot compile

      // Since Scala 2.13 literal types are integrated into the language, so you can write it simply as:
      def f13(x: 120): 120 = x

      println(f13(120))
      "f13(121)" mustNot compile

      // Function narrow narrows type of value 120 from general Int to literal 120.
    }
  }

  "Shapeless singeton type" should {
    "" in {
      trait HttpPort

      def initPort(port: Int with HttpPort): Int with HttpPort =
        port

      initPort(80.asInstanceOf[Int with HttpPort])

      "initPort(80)" mustNot compile

      // Shapeless uses this trick to tag fields and subtypes in an ADT with the singleton types of their names.
      val port/*: Int with KeyTag[String("port"), Int]*/ =
        Symbol("port") ->> 80

      // type FieldType[K, V = V with KeyTag[K, V]
    }
  }
}