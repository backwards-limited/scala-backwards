package tech.backwards.fp.foldable

import tech.backwards.fp.{Disjunction, Id, Just, LeftDisjunction, Maybe, Nothing, RightDisjunction}
import org.scalatest.matchers.must.Matchers
import org.scalatest.wordspec.AnyWordSpec
import tech.backwards.fp._

class FoldableSpec extends AnyWordSpec with Matchers {
  import tech.backwards.fp.foldable.FoldableOps._

  "Foldable" should {
    "work with Id" in {
      import tech.backwards.fp.foldable.IdOps._

      Id("a").foldr("#")((x, acc) => acc + x) mustBe "#a"
    }

    "work with List" in {
      import tech.backwards.fp.foldable.ListOps._

      List("a", "b", "c").foldr("#")((x, acc) => acc + x) mustBe "#cba"
    }

    "work with tuples" in {
      import tech.backwards.fp.foldable.TupleOps._

      ("a", "b").foldr("#")((x, acc) => acc + x) mustBe "#ba"

      ("a", "b", "c").foldr("#")((x, acc) => acc + x) mustBe "#cba"
    }

    "work with Maybe" in {
      import tech.backwards.fp.foldable.MaybeOps._

      Just(1).foldr(0)((x, acc) => acc + x) mustBe 1

      Nothing[Int]().foldr(0)((x, acc) => acc + x) mustBe 0

      val maybe: Maybe[Int] = Just(1)
      maybe.foldr(0)((x, acc) => acc + x) mustBe 1
    }

    "work with Disjunction" in {
      import tech.backwards.fp.foldable.DisjunctionOps._

      RightDisjunction[String, String]("abc").foldr("#")((x, acc) => acc + x) mustBe "#abc"

      LeftDisjunction[String, Int]("foo").foldr("#")((x, acc) => acc + x) mustBe "#"

      val disjunction: Disjunction[String, String] = RightDisjunction[String, String]("abc")
      disjunction.foldr("#")((x, acc) => acc + x) mustBe "#abc"
    }
  }
}