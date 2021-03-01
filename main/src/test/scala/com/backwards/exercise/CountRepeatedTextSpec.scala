package com.backwards.exercise

import org.scalatest.matchers.must.Matchers
import org.scalatest.wordspec.AnyWordSpec

class CountRepeatedTextSpec extends AnyWordSpec with Matchers {
  "Counts" should {
    "count all" in {
      val input: String =
        "aaaabbbcca"

      val output: Map[Char, List[Char]] =
        input.toList.groupBy(identity)

      val counts: List[(String, Int)] =
        output.map { case (c, cs) => c.toString -> cs.size } toList

      counts must contain theSameElementsAs List(("a", 5), ("b", 3), ("c", 2))
    }

    "count only immediate" in {
      val input: String =
        "aaaabbbcca"

      def counts(input: List[String]): List[(String, Int)] = {
        def go(cur: List[String]): List[String] => List[List[String]] = {
          case Nil => List(cur)
          case x :: xs if cur == Nil || cur.contains(x) => go(cur :+ x)(xs)
          case x :: xs => cur +: go(List(x))(xs)
        }

        go(Nil)(input).map(xs => xs.head -> xs.size)
      }

      counts(input.toList.map(_.toString)) must contain theSameElementsAs List(("a", 4), ("b", 3), ("c", 2), ("a", 1))
    }
  }
}