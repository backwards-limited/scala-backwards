package com.backwards.algorithms

import org.scalatest.matchers.must.Matchers
import org.scalatest.wordspec.AnyWordSpec

class MergeSortSpec extends AnyWordSpec with Matchers {
  "Bubble sort" should {
    "be performed non-functional" in {
      def bubbleSort(xs: Array[Int]): Unit =
        for {
          k <- 1 until xs.length
          j <- 0 until xs.length - k
          if xs(j) > xs(j + 1)
        } {
          val x = xs(j)
          xs(j) = xs(j + 1)
          xs(j + 1) = x
        }

      val xs = Array(3, 6, 1, 5, 2, 0, 8)
      bubbleSort(xs)
      println(xs.mkString(", "))
    }
  }
}