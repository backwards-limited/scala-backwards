package tech.backwards.recursion

import scala.util.Try
import org.scalatest.matchers.must.Matchers
import org.scalatest.wordspec.AnyWordSpec

/**
 * [[https://www.47deg.com/blog/basic-recursion-schemes-in-scala/]]
 */
class RecursionSpec extends AnyWordSpec with Matchers {
  "Recursion on a list" should {
    type OCons = Option[(Int, Int)]

    "sum" in {
      def fold(zero: Int, op: (Int, Int) => Int)(list: List[Int]): Int =
        list match {
          case Nil          => zero
          case head :: tail => op(head, fold(zero, op)(tail))
        }

      fold(0, _ + _)(List(1, 2, 3, 4, 5)) mustBe 15
    }

    "fold via ocons" in {
      def fold(out: OCons => Int)(list: List[Int]): Int =
        list match {
          case Nil          => out(None)
          case head :: tail => out(Option(head -> fold(out)(tail)))
        }
    }

    "get the list of digits ([0-9]) of a number (though in reverse order)" in {
      def digitsWithoutUnfold(seed: Int): List[Int] =
        if (seed == 0)
          Nil
        else
          (seed % 10) :: digitsWithoutUnfold(seed / 10)

      digitsWithoutUnfold(250) mustBe List(0, 5, 2)

      def unfold(isEnd: Int => Boolean, op: Int => (Int, Int))(seed: Int): List[Int] =
        if (isEnd(seed))
          Nil
        else {
          val (head, next) = op(seed)
          head :: unfold(isEnd, op)(next)
        }

      def isZero(x: Int): Boolean = x == 0

      def div10(x: Int): (Int, Int) = (x % 10, x / 10)

      def digits(seed: Int): List[Int] = unfold(isZero, div10)(seed)

      digits(250) mustBe List(0, 5, 2)
    }

    "unfold via ocons" in {
      def unfold(into: Int => OCons)(seed: Int): List[Int] =
        into(seed) match {
          case None               => Nil
          case Some((head, next)) => head :: unfold(into)(next)
        }

      /*
      Both fold and unfold now take as parameter a function on OCons, but with reversed types:
      in fold the function gets a number out of an OCons,
      whereas in unfold it splits a number into an OCons.
      */
    }
  }

  "Recursion on a tree" should {
    sealed trait Tree

    final case class Node(left: Tree, top: Int, right: Tree) extends Tree

    final case object Leaf extends Tree

    "sum" in {
      def sum(tree: Tree): Int =
        tree match {
          case Leaf            => 0
          case Node(l, top, r) => sum(l) + top + sum(r)
        }

      sum(Node(Leaf, 3, Node(Leaf, 15, Node(Leaf, 2, Leaf)))) mustBe 20
    }

    "sum with fold" in {
      def fold(zero: Int, op: (Int, Int, Int) => Int)(tree: Tree): Int =
        tree match {
          case Leaf =>
            zero
          case Node(l, top, r) =>
            op(fold(zero, op)(l), top, fold(zero, op)(r))
        }

      def add3(a: Int, b: Int, c: Int): Int = a + b + c

      def sum(tree: Tree): Int = fold(0, add3)(tree)

      sum(Node(Leaf, 3, Node(Leaf, 15, Node(Leaf, 2, Leaf)))) mustBe 20
    }

    // splitNumbers: split a number's digits in the middle,
    // for example, splitNumber(56784197) = (567, 8, 4197)
    def splitNumber(seed: Int): (Int, Int, Int) = {
      def number(n: String): Int = Try(n.toInt).getOrElse(0)

      val (l, r) = seed.toString.toList.splitAt(seed.toString.length / 2)

      if (l.length >= r.length)
        (number(l.init.mkString), number(l.last.toString), number(r.mkString))
      else
        (number(l.mkString), number(r.head.toString), number(r.tail.mkString))
    }

    "unfold without an explicit unfold function" in {
      // We can write a function digits to lay the digits of a large number out in a binary tree
      def digits(seed: Int): Tree =
        if (seed == 0)
          Leaf
        else {
          val (pref, mid, suff) = splitNumber(seed)
          Node(digits(pref), mid, digits(suff))
        }

      digits(56784197) mustBe Node(Node(Node(Leaf, 5, Leaf), 6, Node(Leaf, 7, Leaf)), 8, Node(Node(Leaf, 4, Leaf), 1, Node(Leaf, 9, Node(Leaf, 7, Leaf))))
    }

    "unfold" in {
      // We can extract recursion from the digits function into an unfold functions for trees
      def unfold(isEnd: Int => Boolean, op: Int => (Int, Int, Int))(seed: Int): Tree =
        if (isEnd(seed))
          Leaf
        else {
          val (l, top, r) = op(seed)
          Node(unfold(isEnd, op)(l), top, unfold(isEnd, op)(r))
        }

      def digits(seed: Int): Tree = unfold(_ == 0, splitNumber)(seed)

      digits(56784197) mustBe Node(Node(Node(Leaf, 5, Leaf), 6, Node(Leaf, 7, Leaf)), 8, Node(Node(Leaf, 4, Leaf), 1, Node(Leaf, 9, Node(Leaf, 7, Leaf))))
    }

    type ONode = Option[(Int, Int, Int)]

    "fold via onode" in {
      def fold(out: ONode => Int)(tree: Tree): Int =
        tree match {
          case Leaf =>
            out(None)
          case Node(l, top, r) =>
            out(Some((fold(out)(l), top, fold(out)(r))))
        }
    }

    "unfold via onode" in {
      def unfold(in: Int => ONode)(seed: Int): Tree =
        in(seed) match {
          case None =>
            Leaf
          case Some((l, top, r)) =>
            Node(unfold(in)(l), top, unfold(in)(r))
        }
    }
  }
}