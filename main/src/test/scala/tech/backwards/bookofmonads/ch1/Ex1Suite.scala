package tech.backwards.bookofmonads.ch1

import munit._

/**
 * Monads
 */
class Ex1Suite extends FunSuite {
  test("Count leaves of a Tree") {
    import Ex1Fixture._

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

  test("Relabel tree") {
    import Ex1aFixture._

    assertEquals(
      relabel(
        Node(
          Leaf("a"),
          Node(
            Leaf("b"),
            Leaf("c")
          )
        ),
        10
      ),
      Node(
        Leaf(10 -> "a"),
        Node(
          Leaf(11 -> "b"),
          Leaf(12 -> "c")
        )
      ) -> 13
    )
  }

  test("Relabel tree") {
    import Ex1bFixture._

    assertEquals(
      relabel(
        Node(
          Leaf("a"),
          Node(
            Leaf("b"),
            Leaf("c")
          )
        )
      )(10),
      Node(
        Leaf(10 -> "a"),
        Node(
          Leaf(11 -> "b"),
          Leaf(12 -> "c")
        )
      ) -> 13
    )
  }

  test("Relabel tree") {
    import Ex1cFixture._

    assertEquals(
      relabel(
        Node(
          Leaf("a"),
          Node(
            Leaf("b"),
            Leaf("c")
          )
        )
      )(10),
      Node(
        Leaf(10 -> "a"),
        Node(
          Leaf(11 -> "b"),
          Leaf(12 -> "c")
        )
      ) -> 13
    )
  }

  test("Relabel tree") {
    import Ex1dFixture._

    assertEquals(
      relabel(
        Node(
          Leaf("a"),
          Node(
            Leaf("b"),
            Leaf("c")
          )
        )
      )(10),
      Node(
        Leaf(10 -> "a"),
        Node(
          Leaf(11 -> "b"),
          Leaf(12 -> "c")
        )
      ) -> 13
    )
  }

  test("Relabel tree") {
    import Ex1eFixture._

    assertEquals(
      relabel(
        Node(
          Leaf("a"),
          Node(
            Leaf("b"),
            Leaf("c")
          )
        )
      )(10),
      Node(
        Leaf(10 -> "a"),
        Node(
          Leaf(11 -> "b"),
          Leaf(12 -> "c")
        )
      ) -> 13
    )
  }

  test("Relabel tree") {
    import Ex1fFixture._

    assertEquals(
      relabel(
        Node(
          Leaf("a"),
          Node(
            Leaf("b"),
            Leaf("c")
          )
        )
      )(10),
      Node(
        Leaf(10 -> "a"),
        Node(
          Leaf(11 -> "b"),
          Leaf(12 -> "c")
        )
      ) -> 13
    )
  }
}

trait Ex1Fixture {
  sealed abstract class Tree[A] // Binary trees

  case class Leaf[A](value: A) extends Tree[A]

  case class Node[A](left: Tree[A], right: Tree[A]) extends Tree[A]

  def numberOfLeaves[A](t: Tree[A]): Int =
    t match {
      case Leaf(_) => 1
      case Node(l, r) => numberOfLeaves(l) + numberOfLeaves(r)
    }
}

object Ex1Fixture extends Ex1Fixture

object Ex1aFixture extends Ex1Fixture {
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
}

object Ex1bFixture extends Ex1Fixture {
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
}

object Ex1cFixture extends Ex1Fixture {
  def next[A, B](f: Int => (A, Int))(g: A => Int => (B, Int)): Int => (B, Int) =
    i => {
      val (r, i1) = f(i)
      g(r)(i1)
    }

  def pure[A](x: A): Int => (A, Int) =
    i => (x, i)

  def relabel[A](t: Tree[A]): Int => (Tree[(Int, A)], Int) =
    t match {
      case Leaf(x) =>
        i => (Leaf((i, x)), i + 1)

      case Node(l, r) =>
        next(relabel(l)) { ll =>
          next(relabel(r)) { rr =>
            pure(Node(ll, rr))
          }
        }
    }
}

object Ex1dFixture extends Ex1Fixture {
  implicit class RichWithCounter[A](f: Int => (A, Int)) {
    def next[B](g: A => Int => (B, Int)): Int => (B, Int) =
      i => {
        val (r, i1) = f(i)
        g(r)(i1)
      }
  }

  def pure[A](x: A): Int => (A, Int) =
    i => (x, i)

  def relabel[A](t: Tree[A]): Int => (Tree[(Int, A)], Int) =
    t match {
      case Leaf(x) =>
        i => (Leaf((i, x)), i + 1)

      case Node(l, r) =>
        relabel(l) next { ll =>
          relabel(r) next { rr =>
            pure(Node(ll, rr))
          }
        }
    }
}

object Ex1eFixture extends Ex1Fixture {
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

  def relabel[A](t: Tree[A]): WithCounter[Tree[(Int, A)]] =
    t match {
      case Leaf(x) =>
        i => (Leaf((i, x)), i + 1)

      case Node(l, r) =>
        relabel(l) next { ll =>
          relabel(r) next { rr =>
            pure(Node(ll, rr))
          }
        }
    }
}

/**
 * We say that State adds a context to a value.
 * An element of type `State s a` is not a single value but rather a transformation of a given state into a new state and a result value.
 * We also say that next makes State work in a sequential fashion: the result of one computation is fed to the next computation in the sequence.
 */
object Ex1fFixture extends Ex1Fixture {
  // The following type synonym has two type variables — the first one is the state itself, and the second is the value type:
  type State[S, A] = S => (A, S)

  implicit class RichWithCounter[A](f: State[Int, A]) {
    def next[B](g: A => State[Int, B]): State[Int, B] =
      i => {
        val (r, i1) = f(i)
        g(r)(i1)
      }
  }

  def pure[A](x: A): State[Int, A] =
    i => (x, i)

  def relabel[A](t: Tree[A]): State[Int, Tree[(Int, A)]] =
    t match {
      case Leaf(x) =>
        i => (Leaf((i, x)), i + 1)

      case Node(l, r) =>
        relabel(l) next { ll =>
          relabel(r) next { rr =>
            pure(Node(ll, rr))
          }
        }
    }
}