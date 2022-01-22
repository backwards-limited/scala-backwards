package tech.backwards.theory

import org.scalatest.matchers.must.Matchers
import org.scalatest.wordspec.AnyWordSpec

/**
 * [[https://www.youtube.com/watch?v=g2o22C3CRfU]]
 *
 * Time complexity (i.e. longer it takes to run an algoritm)
 * Space complexity (i.e. algorithm needs more memory)
 *
 * O(n) where O is "order of complexity" and n is "number of inputs".
 */
class BigONotationSpec extends AnyWordSpec with Matchers {
  "Best to worst big O where n is the number of items to iterate" should {
    "O(1) - Constant" in {
      /*
      E.g. look up item in an indexed list by its index giving a completely flat line.
      */
    }

    "O(log n)" in {
      /*
      E.g. a binary search where we reduce as we search so we have a flattening curve.
      */
    }

    "O(n)" in {
      /*
      E.g. iterate over a list - linear
      1 to 1 relationship between the number of inputs and the time it takes to iterate.
      We have a straight x-y line
      */
    }

    "O(n squared)" in {
      /*
      E.g. a loop withing a loop so the curve is going up.
      */
    }

    "O(2 to the n) i.e. exponential" in {
      /*
      E.g. iterating over every possible combination of a list, again a curve going up
      */
    }
  }

  "Simplify a notation down to the worst one" should {
    "combine different notations and choose the worst to discover the complexity" in {
      /*
      E.g.
      O(n squared) + O(log n) == O(n squared)
      */
    }
  }
}