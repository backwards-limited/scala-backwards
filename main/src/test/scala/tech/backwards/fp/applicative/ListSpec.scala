package tech.backwards.fp.applicative

import org.scalatest.matchers.must.Matchers
import org.scalatest.wordspec.AnyWordSpec

class ListSpec extends AnyWordSpec with Matchers {
  import tech.backwards.fp.applicative.ApplicativeOps._
  import tech.backwards.fp.applicative.ListOps._
  import tech.backwards.fp.functor.FunctorOps._
  import tech.backwards.fp.functor.ListOps._

  "List Applicative" should {
    "work for fmap and <*>" in {
      val l: List[Int => (Int, Int)] = List(1, 2) fmap ((x: Int, y: Int) => (x, y)).curried

      val ll: List[(Int, Int)] = l <*> List(3, 4, 5)

      ll mustBe List(
        (1, 3), (1, 4), (1, 5),
        (2, 3), (2, 4), (2, 5)
      )

      // Altogether
      (List(1, 2) fmap ((x: Int) => (y: Int) => (x, y))) <*> List(3, 4, 5) mustBe List(
        (1, 3), (1, 4), (1, 5),
        (2, 3), (2, 4), (2, 5)
      )
    }

    "work for <$> and <*>" in {
      val l: List[Int => (Int, Int)] = ((x: Int, y: Int) => (x, y)).curried `<$>` List(1, 2)

      val ll: List[(Int, Int)] = l <*> List(3, 4, 5)

      ll mustBe List(
        (1, 3), (1, 4), (1, 5),
        (2, 3), (2, 4), (2, 5)
      )

      // Altogether
      (((x: Int, y: Int) => (x, y)).curried `<$>` List(1, 2)) <*> List(3, 4, 5) mustBe List(
        (1, 3), (1, 4), (1, 5),
        (2, 3), (2, 4), (2, 5)
      )
    }
  }
}