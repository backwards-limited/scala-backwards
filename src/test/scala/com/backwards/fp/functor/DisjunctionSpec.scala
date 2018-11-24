package com.backwards.fp.functor

import org.scalatest.{MustMatchers, WordSpec}
import com.backwards.fp.Disjunction._
import com.backwards.fp.{Disjunction, LeftDisjunction, RightDisjunction}

class DisjunctionSpec extends WordSpec with MustMatchers {
  import com.backwards.fp.functor.DisjunctionOps._

  "Disjunction/Either Functor" should {
    "works with left" in {
      LeftDisjunction[String, Int]("foo") fmap { x => s"${x}bar" } mustBe LeftDisjunction[String, Int]("foo")

      left[String, Int]("foo") fmap { x => s"${x}bar" } mustBe LeftDisjunction[String, Int]("foo")
    }

    "works with right" in {
      RightDisjunction[String, Int](10) fmap { x => x + 20 } mustBe RightDisjunction[String, Int](30)

      right[String, Int](10) fmap { x => x + 20 } mustBe RightDisjunction[String, Int](30)
    }

    "works with disjunction" in {
      val d: Disjunction[String, Int] = RightDisjunction[String, Int](10)

      d fmap { x => x + 20 } mustBe RightDisjunction[String, Int](30)
    }

    "left obeys identity" in {
      val l = left[String, Int]("error")

      l fmap identity mustBe l
    }

    "right obeys identity" in {
      val r = right[String, Int](10)

      r fmap identity mustBe r
    }

    "obeys composition" in {
      val f = { x: Int => x + 10 }
      val g = { x: Int => x * 2 }
      val r = right[String, Int](10)

      { r fmap f fmap g } mustBe { r fmap { f andThen g } }
    }
  }
}