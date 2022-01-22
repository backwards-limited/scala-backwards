package tech.backwards.typelevel

import shapeless.Witness.Lt
import org.scalatest.matchers.must.Matchers
import org.scalatest.wordspec.AnyWordSpec
import shapeless._
import shapeless.syntax.SingletonOps

/**
 * @see [[https://harrylaou.com/slides/shapeless101.pdf]]
 */
class ShapelessSpec extends AnyWordSpec with Matchers {
  "Type level programming" should {
    "be done with Shapeless" in {
      def bla(v: String): String = ???
      // v is value parameter

      def bla2[A](v: A): A = ???
      // v is value parameter and A is type parameter

      // Heterogenous Lists:
      // A List containing elements of different types and retains the type of each element in its own type
      val hlist: Int :: String :: HNil =
        26 :: "Harry" :: HNil

      case class Person(name:String, age:Int)

      val gen = Generic[Person]

      val harry = Person("Harry", 39)
      val hl: String :: Int :: HNil = gen to harry
      val p: Person = gen from hl

      harry mustBe p
    }

    "delve into higher kinds and type constructors" in {
      /*
      List is a type constructor, it takes one type parameter
      notation: * -> *

      Type constructor examples:
      Option
      Future
      F[_]

      List[String] is a type, produced by a type constructor List using type
      parameter String
      notation: *

      Type examples:
      Option[Int]
      Future[String]
      F[A]
      */
    }

    "delve into natural transformations" in {
      /*
      Natural transformations are similar to functions e.g.

      Function1:
      val f: A => B

      Natural transformation:
      val t: F ~> G

      or
      val t: F[A] => G[A]
      */

      import shapeless.poly._

      object choose extends (Set ~> Option) {
        def apply[T](s : Set[T]): Option[T] = s.headOption
      }

      val x: Option[Int] = choose(Set(1, 2, 3))
      x mustBe Some(1)
    }

    "delve into polymorphic functions" in {
      object size extends Poly1 {
        implicit def caseInt = at[Int](x => 1)

        implicit def caseString = at[String](_.length)

        implicit def caseTuple[T, U](implicit st: Case.Aux[T, Int], su: Case.Aux[U, String]) =
          at[(T, U)] { case (first, second) => size(first) + size(second) }
      }

      size(23) mustBe 1

      size("foo") mustBe 3

      // size((23, "foo")) mustBe 4 TODO - Not compiling

      object plus extends Poly2 {
        implicit val caseInt = at[Int,Int](_+_)

        implicit val caseDouble = at[Double,Double](_+_)

        implicit val caseString = at[String,String](_+_)

        implicit def caseList[T] = at[List[T],List[T]](_:::_)
      }

      plus(1, 1) mustBe 2

      plus("foo", "bar") mustBe "foobar"
    }

    "delve into Singleton an Witness" in {
      val a: Int = 26
      // Yes, an Int, but internally in scalac represented as Int(26)

      import shapeless.syntax.singleton._

      val b = 26.narrow
      // b is of type Int(26) which is a subclass of Int i.e. Int(26) <: Int
      // A singleton type is a type for which there exists exactly on value e.g. Int(26).
      // We can lift this value in the type system - To get a value from a singleton type we use Witness.

      val c: Lt[Int] = Witness(26)

      val d: (Witness {
        type T <: Int
      })#T = c.value
    }

    "delve into LabelledGeneric" in {
      case class Rectangle(width: Int, height: Int)

      val gen = LabelledGeneric[Rectangle]
      val hlist = gen.to(Rectangle(1, 3))

      // hlist.get(Symbol("width"))

      import shapeless.ops.record._

      case class Foo(bar: String, baz: Boolean)

      val labl = LabelledGeneric[Foo]
      val keys = Keys[labl.Repr].apply()

      keys mustBe Symbol("bar") :: Symbol("baz") :: HNil
    }

    "convert ADT to Map" in {
      case class X(a: Boolean, b: String, c: Int)

      case class Y(a: String, b: String)

      import shapeless.ops.product._
      import shapeless.syntax.std.product._

      object X {
        implicit val lgenX = LabelledGeneric[X]
      }

      object Y {
        implicit val lgenY = LabelledGeneric[Y]
      }

      // Define a typeclass to provide the toMap methods
      object ToMapImplicits {
        implicit class ToMapOps2[A <: Product](a: A) {
          def mkMap(implicit T: ToMap.Aux[A, Symbol, Any]): Map[String, String] =
            a.toMap[Symbol, Any].map { case (k: Symbol, v) =>
              k.name -> v.toString
            }
        }
      }

      import ToMapImplicits._

      val x: X = X(a = true, "bike", 26)
      val y: Y = Y("first", "second")

      val mapX: Map[String, String] = x.mkMap
      val mapY: Map[String, String] = y.mkMap

      println("mapX = " + mapX)
      println("mapY = " + mapY)
    }
  }
}