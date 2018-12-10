package com.backwards.fp.applicative

import org.scalatest.{MustMatchers, WordSpec}
import com.backwards.fp.{Disjunction, LeftDisjunction, RightDisjunction}

class DisjunctionSpec extends WordSpec with MustMatchers {
  import com.backwards.fp.functor.DisjunctionOps._
  import com.backwards.fp.applicative.DisjunctionOps._

  "Right Disjunction Applicative" should {
    "work using fmap and <*> separately" in {
      val `fmapped right 10`: Disjunction[String, Int => Int => Int] =
        RightDisjunction[String, Int](10) fmap ((x: Int, y: Int, z: Int) => x + y + z).curried

      val `apped right 20`: Disjunction[String, Int => Int] =
        `fmapped right 10` <*> RightDisjunction(20)

      `apped right 20` <*> RightDisjunction(30) mustBe RightDisjunction(10 + 20 + 30)
    }

    "work using fmap and <*> together" in {
      val result = ((RightDisjunction[String, Int](10) fmap ((x: Int) => (y: Int) => (z: Int) => x + y + z)) <*> RightDisjunction(20)) <*> RightDisjunction(30)

      result mustBe RightDisjunction(10 + 20 + 30)
    }

    "work using <$> and <*> separately" in {
      val `fmapped right 10`: Disjunction[String, Int => Int => Int] =
        ((x: Int, y: Int, z: Int) => x + y + z).curried `<$>` RightDisjunction[String, Int](10)

      val `apped right 20`: Disjunction[String, Int => Int] =
        `fmapped right 10` <*> RightDisjunction(20)

      `apped right 20` <*> RightDisjunction(30) mustBe RightDisjunction(10 + 20 + 30)
    }

    "work using <$> and <*> together" in {
      val result = ((((x: Int) => (y: Int) => (z: Int) => x + y + z) `<$>` RightDisjunction[String, Int](10)) <*> RightDisjunction(20)) <*> RightDisjunction(30)

      result mustBe RightDisjunction(10 + 20 + 30)
    }
  }

  "Left Disjunction Applicative" should {
    "work using fmap and <*> separately" in {
      val `fmapped right 10`: Disjunction[String, Int => Int => Int] =
        RightDisjunction[String, Int](10) fmap ((x: Int, y: Int, z: Int) => x + y + z).curried

      val `apped left`: Disjunction[String, Int => Int] =
        `fmapped right 10` <*> LeftDisjunction("whoops")

      `apped left` <*> RightDisjunction(30) mustBe LeftDisjunction("whoops")
    }

    "work using fmap and <*> together" in {
      val result = ((RightDisjunction[String, Int](10) fmap ((x: Int) => (y: Int) => (z: Int) => x + y + z)) <*> LeftDisjunction("whoops")) <*> RightDisjunction(30)

      result mustBe LeftDisjunction("whoops")
    }

    "work using <$> and <*> separately" in {
      val `fmapped right 10`: Disjunction[String, Int => Int => Int] =
        ((x: Int, y: Int, z: Int) => x + y + z).curried `<$>` RightDisjunction[String, Int](10)

      val `apped left`: Disjunction[String, Int => Int] =
        `fmapped right 10` <*> LeftDisjunction("whoops")

      `apped left` <*> RightDisjunction(30) mustBe LeftDisjunction("whoops")
    }

    "work using <$> and <*> together" in {
      val result = ((((x: Int) => (y: Int) => (z: Int) => x + y + z) `<$>` RightDisjunction[String, Int](10)) <*> LeftDisjunction("whoops")) <*> RightDisjunction(30)

      result mustBe LeftDisjunction("whoops")
    }
  }
}