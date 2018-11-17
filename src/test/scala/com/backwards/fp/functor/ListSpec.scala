package com.backwards.fp.functor

import org.scalatest.{MustMatchers, WordSpec}

class ListSpec extends WordSpec with MustMatchers {
  import FunctorOps._

  implicit val listFunctor: Functor[List] = new Functor[List] {
    def fmap[A, B](fa: List[A])(f: A => B): List[B] = {
      lazy val go: List[A] => List[B] = {
        case Nil => Nil
        case h +: t => f(h) +: go(t)
      }

      go(fa)
    }
  }

  "List functor" should {
    "work on simple functions" in {
      List(1) fmap { x: Int => x + 1 } mustBe List(2)

      List(1, 2, 3) fmap { x: Int => x + 1 } mustBe List(2, 3, 4)
    }

    "obey identity" in {
      List(1) fmap identity mustBe List(1)
    }

    "obey composition" in {
      val f = { x: Int => x + 1 }
      val g = { x: Int => x.toString + "a" }

      { List(1) fmap f fmap g } mustBe { List(1) fmap { f andThen g } }
    }
  }
}