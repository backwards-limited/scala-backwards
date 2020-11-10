package com.backwards.exercise

import scala.annotation.tailrec
import scala.collection.MapView
import scala.util.chaining.scalaUtilChainingOps
import cats.implicits.{catsSyntaxOptionId, none}
import org.scalatest.matchers.must.Matchers
import org.scalatest.wordspec.AnyWordSpec

class UniqueLetterInWordsSpec extends AnyWordSpec with Matchers {
  "Words" should {
    "be counted" in {
      def count(lines: Seq[String]): Unit = {
        val result: MapView[String, Int] = lines
          .flatMap(x => x.split("\\s"))
          .map(x => x.toLowerCase)
          .groupBy(x => x)
          .view
          .mapValues(x => x.size)

        println(result.toList)
      }

      count(List("I", "am", "walking", "the", "dog", "next", "to", "the", "dog", "playground"))
    }
  }

  "Find unique letter" should {
    "be done recursively for complexity of O(n squared)" in {
      /*
      def find(xs: Seq[Char]): Option[Char] = {
        @tailrec
        def go(xs: Seq[Char]): Option[Char] = xs match {
          case x :: xs if xs contains x => go(xs)
          case x :: _ => Option(x)
          case Nil => None
        }

        go(xs)
      }*/

      lazy val find: Seq[Char] => Option[Char] = {
        case x :: xs if xs contains x => find(xs)
        case x :: _ => Option(x)
        case Nil => None
      }

      find("abcdefghijk".toList) mustBe Option('a')
      find("aBcaBdefgBae".toList) mustBe Option('c')
    }

    "be done with 2 O(n) complexities" in {
      def find(xs: List[Char]): Option[Char] = {
        @tailrec
        def go(xs: List[(Char, Int)], m: Map[Char, List[Int]]): Map[Char, List[Int]] = xs match {
          case (c, i) :: xs => go(xs, m.updatedWith(c)(_.fold(List(i).some)(_ :+ i pipe(_.some))))
          case Nil => m
        }

        go(xs.zipWithIndex, Map.empty).foldLeft(none[(Char, Int)]) {
          case Tuple2(s @ Some((cc, ii)), (c, List(i))) => if (ii <= i) s else (c, i).some
          case Tuple2(None, (c, List(i))) => (c, i).some
          case Tuple2(x, _) => x
        }.map(_._1)
      }

      find("abcdefghijk".toList) mustBe Option('a')
      find("aBcaBdefgBae".toList) mustBe Option('c')
    }
  }
}