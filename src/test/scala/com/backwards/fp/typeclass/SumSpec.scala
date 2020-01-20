package com.backwards.fp.typeclass

import org.scalatest.matchers.must.Matchers
import org.scalatest.wordspec.AnyWordSpec
import com.backwards.fp.typeclass.Sum.ops._
import com.backwards.fp.typeclass.SumDifferentTypes.ops._

class SumSpec extends AnyWordSpec with Matchers {
  "Sum" should {
    "sum two ints" in {
      1 |+| 2 mustBe 3
    }

    "sum many ints" in {
      1 |+| 2 |+| 3 mustBe 6
    }

    "sum list of ints" in {
      List(1, 2, 3).foldLeft(0)(_ |+| _) mustBe 6
    }

    "map over list of ints" in {
      List(1, 2, 3).map(_ |+| 2) mustBe List(3, 4, 5)
    }

    "concatenate strings" in {
      "hello" |+| "world" mustBe "helloworld"
    }

    "do wierd things for this specific example" in {
      5 |+| "hello" mustBe true
    }

    "behave like andThen for functions" in {
      val f1: String => Int = _.length
      val f2: Int => String = "hello" * _
      val f3: String => String = f1 |+| f2

      f3("..") mustBe "hellohello"
    }

    "behave like andThen for more functions" in {
      val f1: String => Int = _.length
      val f2: Int => String = "hello" * _
      val f3: String => Int = f1 |+| f2 |+| f1

      f3("..") mustBe 10
    }

    "add each elemment respectively for product types" in {
      case class Foo(a: Int, b: Int, c: String)

      Foo(10, 100, "foo") |+| Foo(1, 11, "bar") mustBe Foo(11, 111, "foobar")
    }

    "add values in option for coproduct types" in {
      Some(2) |+| Some(5) mustBe Some(7)
    }
  }

  "Sum and Eq" should {
    "work properly together" in {
      import com.backwards.fp.typeclass.Eq.ops._

      1 |+| 2 |+| 3 |=| 6 mustBe true
    }  
  }
}