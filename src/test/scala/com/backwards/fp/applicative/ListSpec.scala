package com.backwards.fp.applicative

import org.scalatest.{MustMatchers, WordSpec}

class ListSpec extends WordSpec with MustMatchers {
  "List Applicative" should {
    "work for fmap and <*>" in {
      import com.backwards.fp.functor.FunctorOps._
      import com.backwards.fp.functor.ListOps._

      val v: List[Int => (Int, Int)] = List(1, 2) fmap ((x: Int, y: Int) => (x, y)).curried
    }

    "work for <$> and <*>" in {

    }
  }
}