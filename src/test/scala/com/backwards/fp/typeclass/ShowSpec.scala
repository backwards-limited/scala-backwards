package com.backwards.fp.typeclass

import org.scalatest._
import com.backwards.fp.typeclass.Show.ops._

class ShowSpec extends WordSpec with MustMatchers {
  case class Foo(foo: Int)

  case class Bar(b: Int, a: String, r: Int)

  "Show" should {
    "work when called as function" in {
      show(20) mustBe "int 20"
    }

    "work when called with dot notation" in {
      20.show mustBe "int 20"
    }

    "work with strings" in {
      "foofoo".show mustBe "string foofoo"
    }

    "work with case classes (products)" in {
      Foo(42).show mustBe "ShowSpec.this.Foo :: foo: int 42"

      Bar(123, "11", 42).show mustBe "ShowSpec.this.Bar :: b: int 123, a: string 11, r: int 42"
    }

    "allow instance override by implicit precedence" in {
      implicit val hipsterString: Show[String] =
        str => s"""hipster string $str"""

      "bazbaz".show mustBe "hipster string bazbaz"
    }
  }

  "Show our own ADTs i.e. products and coproducts" should {
    "show a Foo" in {
      implicit val fooShow: Show[Foo] =
        foo => s"case class Foo(foo: ${foo.foo})"

      30.show mustBe "int 30"
      Foo(42).show mustBe "case class Foo(foo: 42)"
    }
  }
}