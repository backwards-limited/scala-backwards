package com.backwards.fp.typeclass

import org.scalatest.matchers.must.Matchers
import org.scalatest.wordspec.AnyWordSpec
import com.backwards.fp.typeclass.Eq.ops._

class EqSpec extends AnyWordSpec with Matchers {
  sealed trait Base
  
  sealed trait Baz extends Base
  
  case class Foo1(foo: Int) extends Baz
  
  case class Bar(b: Int, a: String, r: Int) extends Baz
  
  case class Foo2(foo: Int) extends Base

  "|=|" should {
    "work for ints" in {
      14 |=| 13 mustBe false
      14 |=| 14 mustBe true
    }

    "compare product types properly" in {
      Foo1(10) |=| Foo1(20) mustBe false
      Foo1(20) |=| Foo1(20) mustBe true
      Bar(20, "bar", 30) |=| Bar(20, "bar", 30) mustBe true
    }

    "compare coproduct types properly" in {
      val foo1: Base = Foo1(10)
      val foo2: Base = Foo1(20)
      val foo3: Base = Foo2(10)

      foo1 |=| foo2 mustBe false
      foo1 |=| foo3 mustBe false
    }

    "make possible to use result as parameter in comparison" in {
      true |=| false |=| false mustBe true
    }

    "compare Option properly, as it is coproduct" in {
      val s10: Option[Int] = Some(10)
      val s2 = Some(2)
      val n: Option[Int] = None

      s10 |=| n mustBe false
      s10 |=| s2 mustBe false
      s10 |=| s10 mustBe true
    }

    "compare Either properly, as it is coproduct" in {
      val a: Either[String, Int] = Right(10)
      val b: Either[String, Int] = Left("error")
      val c: Either[String, Int] = Right(20)
      val d: Either[String, Int] = Right(10)

      a |=| b mustBe false
      a |=| c mustBe false
      a |=| d mustBe true
    }

    "compare futures" in {
      import scala.concurrent.ExecutionContext.Implicits.global
      import scala.concurrent.Future

      val f1: Future[Int] = Future(10)
      val f2: Future[Int] = Future(20)
      val f3: Future[Int] = Future(10)

      f1 |=| f2 mustBe false
      f1 |=| f3 mustBe true
    }

    "allow making jokes ;p, using other implicit instances than default" in {
      implicit val intJokeEq: Eq[Int] =
        (a: Int, b: Int) => if (b == 13) a == b else a != b

      14 |=| 13 mustBe false
      14 |=| 14 mustBe false
    }

    "compare tuples" in {
      (10, "foo") |=| (10, "foo") mustBe true
      (10, "foo", 11, true) |=| (10, "foo", 11, true) mustBe true
      (10, "foo") |=| (10, "bar") mustBe false
    }
  }
}