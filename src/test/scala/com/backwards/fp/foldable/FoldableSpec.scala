package com.backwards.fp.foldable

import org.scalatest.{MustMatchers, WordSpec}
import com.backwards.fp.Id

class FoldableSpec extends WordSpec with MustMatchers {
  import com.backwards.fp.foldable.FoldableOps._

  "Foldable" should {
    "work with Id" in {
      import com.backwards.fp.foldable.IdOps._

      Id("a").foldr("#")((x, acc) => acc + x) mustBe "#a"
    }

    "work with List" in {
      import com.backwards.fp.foldable.ListOps._

      List("a", "b", "c").foldr("#")((x, acc) => acc + x) mustBe "#cba"
    }

    "work with tuples" in {
      import com.backwards.fp.foldable.TupleOps._

      ("a", "b").foldr("#")((x, acc) => acc + x) mustBe "#ba"

      ("a", "b", "c").foldr("#")((x, acc) => acc + x) mustBe "#cba"
    }
  }
}