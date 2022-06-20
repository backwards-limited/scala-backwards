package tech.backwards.bookofmonads.ch2

import cats.implicits._
import monocle.Lens
import monocle.macros.GenLens
import munit._

/**
 * Better notation - including rewrite of previous code to use Monad
 */
class Ex1Suite extends FunSuite {
  sealed abstract class Tree[A]

  case class Leaf[A](value: A) extends Tree[A]

  case class Node[A](left: Tree[A], right: Tree[A]) extends Tree[A]

  test("Tree relabel original without Monad") {
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

    assertEquals(
      relabel(
        Node(
          Leaf("a"),
          Node(Leaf("b"), Leaf("c"))
        )
      )(10),
      Node(
        Leaf(10 -> "a"),
        Node(Leaf(11 -> "b"), Leaf(12 -> "c"))
      ) -> 13
    )
  }

  test("Tree relabel original with Monad") {
    type State[S, A] = S => (A, S)

    trait Monad[M[_]] {
      def point[A](x: A): M[A]

      def bind[A, B](x: M[A])(f: A => M[B]): M[B]
    }

    object State {
      implicit def monadState[S]: Monad[State[S, *]] =
        new Monad[State[S, *]] {
          def point[A](x: A): State[S, A] =
            s => (x, s)

          def bind[A, B](state: State[S, A])(f: A => State[S, B]): State[S, B] =
            s => {
              val (a, s1) = state(s)
              f(a)(s1)
            }
        }

      def point[S, A](a: A): State[S, A] =
        s => (a, s)

      implicit class StateExtension[S, A](state: State[S, A]) {
        def bind[B](f: A => State[S, B]): State[S, B] =
          monadState.bind(state)(f)
      }
    }

    import State._

    def relabel[A](t: Tree[A]): State[Int, Tree[(Int, A)]] =
      t match {
        case Leaf(x) =>
          i => (Leaf((i, x)), i + 1)

        case Node(l, r) =>
          relabel(l) bind { ll =>
            relabel(r) bind { rr =>
              point(Node(ll, rr)) } }
      }

    assertEquals(
      relabel(
        Node(
          Leaf("a"),
          Node(Leaf("b"), Leaf("c"))
        )
      )(10),
      Node(
        Leaf(10 -> "a"),
        Node(Leaf(11 -> "b"), Leaf(12 -> "c"))
      ) -> 13
    )
  }
}