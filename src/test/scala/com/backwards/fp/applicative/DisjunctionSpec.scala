package com.backwards.fp.applicative

import org.scalatest.{MustMatchers, WordSpec}
import com.backwards.fp.{Disjunction, RightDisjunction}

class DisjunctionSpec extends WordSpec with MustMatchers {
  import com.backwards.fp.functor.DisjunctionOps._
  //import com.backwards.fp.applicative.ApplicativeOps._
  import com.backwards.fp.applicative.DisjunctionOps._

  "Disjunction Applicative" should {
    "work with right" in {
      val `fmapped right 10`: Disjunction[String, Int => Int => Int] =
        RightDisjunction[String, Int](10) fmap ((x: Int, y: Int, z: Int) => x + y + z).curried

      val `apped right 20` = `fmapped right 10` <*> RightDisjunction(20)
    }
  }
}