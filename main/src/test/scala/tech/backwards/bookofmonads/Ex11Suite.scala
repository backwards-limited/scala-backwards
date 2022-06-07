package tech.backwards.bookofmonads

import munit._

/**
 * Monads
 */
class Ex11Suite extends FunSuite {
  test("Count leaves of a Tree") {
    import Ex11PlumbingFixture._

    assertEquals(
      numberOfLeaves(
        Node(
          Leaf("a"),
          Node(
            Leaf("b"),
            Node(
              Leaf("c"),
              Leaf("d")
            )
          )
        )
      ),
      4
    )
  }
}

trait Ex11Fixture {
  sealed abstract class Tree[A] // Binary trees

  case class Leaf[A](value: A) extends Tree[A]

  case class Node[A](left: Tree[A], right: Tree[A]) extends Tree[A]

  def numberOfLeaves[A](t: Tree[A]): Int =
    t match {
      case Leaf(_) => 1
      case Node(l, r) => numberOfLeaves(l) + numberOfLeaves(r)
    }
}

object Ex11PlumbingFixture extends Ex11Fixture {
  /**
   * Relabel the leaves of the tree left-to-right.
   * Start with a tree t, the result of relabel should contain the same elements,
   * but with each one paired with the index it would receive if the leaves of t were flattened into a list
   * starting with the leftmost leaf and ending with the rightmost one.
   * {{{
   *             *            ------->            *
   *          *     z                          *     (3, z)
   *       x     y                      (1, x)   (2, y)
   * }}}
   * First attempt (see Haskell alternative for more explanation):
   */
  def relabel[A](t: Tree[A], i: Int): (Tree[(Int, A)], Int) =
    t match {
      case Leaf(x) =>
        (Leaf((i, x)), i + 1)

      case Node(l, r) =>
        val (ll, i1) = relabel(l, i)
        val (rr, i2) = relabel(r, i1)
        (Node(ll, rr), i2)
    }

  /*
  def relabel[A](t: Tree[A]): Int => (Tree[(Int, A)], Int) =
    t match {
      case Leaf(x) =>
        i => (Leaf((i, x)), i + 1)

      case Node(l, r) =>
        i => {
          val (ll, i1) = relabel(l)(i)
          val (rr, i2) = relabel(r)(i1)
          (Node(ll, rr), i2)
        }
    }
  */
}

object Ex11WithoutPlumbingHalfwayHouseToStateButWithoutTypeAliasFixture extends Ex11Fixture {
  implicit class RichWithCounter[A](f: Int => (A, Int)) {
    def next[B](g: A => Int => (B, Int)): Int => (B, Int) =
      i => {
        val (r, i1) = f(i)
        g(r)(i1)
      }
  }

  /*
  implicit class RichWithCounter[Tree[(A, Int)]](f: Int => (Tree[(A, Int)], Int)) {
    def next[B](g: Tree[(A, Int)] => Int => (Tree[(B, Int)], Int)): Int => (Tree[(B, Int)], Int) =
      i => {
        val (r, i1) = f(i)
        g(r)(i1)
      }
  }
  */

  def pure[A](x: A): Int => (A, Int) =
    i => (x, i)

  def relabel[A](t: Tree[A]): Int => (Tree[(A, Int)], Int) =
    t match {
      case Leaf(x) =>
        i => (Leaf((x, i)), i + 1)

      case Node(l, r) =>
        relabel(l) next { ll =>
          relabel(r) next { rr =>
            pure(Node(ll, rr))
          }
        }
    }
}

object Ex11WithoutPlumbingHalfwayHouseToStateWithTypeAliasFixture extends Ex11Fixture {
  type WithCounter[A] = Int => (A, Int)

  implicit class RichWithCounter[A](f: WithCounter[A]) {
    def next[B](g: A => WithCounter[B]): WithCounter[B] =
      i => {
        val (r, i1) = f(i)
        g(r)(i1)
      }
  }

  def pure[A](x: A): WithCounter[A] =
    i => (x, i)

  def relabel[A](t: Tree[A]): WithCounter[Tree[(A, Int)]] =
    t match {
      case Leaf(x) =>
        i => (Leaf((x, i)), i + 1)

      case Node(l, r) =>
        relabel(l) next { ll =>
          relabel(r) next { rr =>
            pure(Node(ll, rr))
          }
        }
    }
}