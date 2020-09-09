package com.backwards.typelevel

import cats.Monoid
import org.scalatest.matchers.must.Matchers
import org.scalatest.wordspec.AnyWordSpec
import cats.implicits._
import shapeless.Nat._0
import shapeless.{Generic, HList, Nat, Succ}
import shapeless.ops.hlist.Length

/**
 * @see [[https://gigiigig.github.io/posts/2015/09/13/aux-pattern.html]]
 *
 * The Aux pattern is not a pattern, is a technique used in every library that is doing some type level programming that we need to use to overcome one Scala limitation.
 */
class AuxSpec extends AnyWordSpec with Matchers {
  "Aux" should {
    "be used as an alias for a type-level trait/class" in {
      // Every time we do a type level computation in Scala we have a type alias defined inside another class/trait
      trait Foo[A] {
        type B

        def value: B
      }
      // In this case for instance the result of our type level computation will be stored in B

      // Examples:
      implicit def fi: Foo[Int] {
        type B = String
      } = new Foo[Int] {
        type B = String

        val value = "Foo"
      }

      implicit def fs: Foo[String] {
        type B = Boolean
      } = new Foo[String] {
        type B = Boolean

        val value = false
      }

      // In Scala we can use parameter dependent types to access the type defined inside a class/trait (path dependent type) so if we want to use our type B in a function, as a return type:
      def foo[T](t: T)(implicit F: Foo[T]): F.B = F.value

      val res1: String = foo(2)
      res1 mustBe "Foo"

      val res2: Boolean = foo("")
      res2 mustBe false

      // Now the following will not compile.
      // Let's say we want to use the first (in this case implicit) type in another type but in the same parameter list e.g. get the Monoid instance for said type
      "def foo2[T](t: T)(implicit F: Foo[T], M: Monoid[F.B]): F.B = M.empty" mustNot compile

      // The above would give the compilation error:
      // illegal dependent method type: parameter may only be referenced in a subsequent parameter section
      //      def foo2[T](t: T)(implicit F: Foo[T], M: Monoid[F.B]): Foo.B = M.empty

      // Scala tells us that we can’t use the dependent type in the same section, we can use it in the next parameters block or as a return type only.
      // Here is where our friend Aux is going to help - let's define in companion object:
      object Foo {
        type Aux[A0, B0] = Foo[A0] {
          type B = B0
        }
      }

      // We are defining a type alias where A0 is mapped to Foo A and B0 is mapped to type B.
      // The relation type B = B0 works both ways, so if we fix the type for B like with type B = Boolean, B0 will get this type too.

      // Now we can foo2:
      def foo2[T, R](t: T)(implicit F: Foo.Aux[T, R], M: Monoid[R]): R = M.empty

      val res3: String = foo2(2)
      res3 mustBe ""

      implicit val booleanXorMonoid: Monoid[Boolean] = new Monoid[Boolean] {
        def empty: Boolean = true

        def combine(x: Boolean, y: Boolean): Boolean = (!x || y) && (x || !y)
      }

      val res4 = foo2("")
      res4 mustBe true
    }

    "show a more indepth example" in {
      def length[T, R <: HList](t: T)(implicit G: Generic.Aux[T, R], L: Length[R]): L.Out =
        L()

      case class Foo(i: Int, s: String, b: Boolean)

      val foo: Foo = Foo(1, "", b = false)

      val res = length(foo)
      Nat.toInt(res) mustBe 3
    }
  }
}