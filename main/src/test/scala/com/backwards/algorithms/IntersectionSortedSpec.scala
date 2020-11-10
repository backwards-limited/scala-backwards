package com.backwards.algorithms

import org.scalatest.matchers.must.Matchers
import org.scalatest.wordspec.AnyWordSpec

class IntersectionSortedSpec extends AnyWordSpec with Matchers {
  "Intersection of 2 sorted lists" should {
    "be performed non-functionally" in {
      /**
       * If the given lists are sorted then
       * Complexity: O(n log n)
       *
       * If they are not sorted then
       * Complexity: O(n)
       *
       * Why is the first O(n log n) ?
       * We combine complexities and take the worst e.g. first we have to "merge sort" and then perform the intersection:
       * one = mergeSort(one) <- Complexity O(n log n)
       * two = mergeSort(two) <- Complexity O(n log n)
       *
       * intersection(one, two) <- Complexity O(n) but combined with the previous sorting before calling this:
       * O(n + n log n + n log n) and we then take the worst giving: O(n log n)
       */
      def intersection(one: List[Int], two: List[Int]): List[Int] = {
        var result = List.empty[Int]
        var i, j = 0

        while (i < one.length && j < one.length) {
          if (one(i) == two(j)) {
            result = result :+ one(i)
            i = i + 1
            j = j + 1
          } else if (one(i) > two(j)) {
            j = j + 1
          } else {
            i = i + 1
          }
        }

        result
      }
    }
  }
}