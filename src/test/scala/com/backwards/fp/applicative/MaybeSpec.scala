package com.backwards.fp.applicative

import org.scalatest.{MustMatchers, WordSpec}
import com.backwards.fp.{Just, Nothing}

/**
  * Note that in some cases we use extra parentheses to keep IntelliJ happy.
  */
class MaybeSpec extends WordSpec with MustMatchers {
  import com.backwards.fp.applicative.MaybeOps._
  import com.backwards.fp.functor.FunctorOps._
  import com.backwards.fp.functor.MaybeOps._

  "Maybe applicative of Just" should {
    "work with fmap and <*>" in {
      (Just(5) fmap ((x: Int) => (y: Int) => x + y)) <*> Just(10) mustBe Just(15)
    }

    "work with <$> and <*>" in {
      (((x: Int, y: Int) => x + y).curried `<$>` Just(5)) <*> Just(10) mustBe Just(15)

      (((x: Int) => (y: Int) => x + y) `<$>` Just(5)) <*> Just(10) mustBe Just(15)
    }
  }

  "Maybe applicative of Nothing" should {
    "work with fmap and <*>" in {
      (Just(5) fmap ((x: Int) => (y: Int) => x + y)) <*> Nothing[Int]() mustBe Nothing[Int]()
    }

    "work with <$> and <*>" in {
      (((x: Int, y: Int) =>  x + y).curried `<$>` Nothing[Int]()) <*> Just(10) mustBe Nothing[Int]()

      (((x: Int) => (y: Int) => x + y) `<$>` Nothing[Int]()) <*> Just(5) mustBe Nothing[Int]()
    }
  }
}