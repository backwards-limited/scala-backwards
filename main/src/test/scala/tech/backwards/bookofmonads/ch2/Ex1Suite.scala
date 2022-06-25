package tech.backwards.bookofmonads.ch2

import cats.implicits._
import munit._

/**
 * Better notation - including rewrite of previous code to use Monad
 */
class Ex1Suite extends FunSuite {
  trait Monad[M[_]] {
    def point[A](x: A): M[A]

    def bind[A, B](x: M[A])(f: A => M[B]): M[B]
  }

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
    type State[S, A] = S => (S, A)

    def monadState[S]: Monad[State[S, *]] =
      new Monad[State[S, *]] {
        def point[A](a: A): State[S, A] =
          s => (s, a)

        def bind[A, B](state: State[S, A])(f: A => State[S, B]): State[S, B] =
          s => {
            val (s1, a) = state(s)
            f(a)(s1)
          }
      }

    def point[S, A](a: A): State[S, A] =
      monadState.point(a)

    implicit class StateExtension[S, A](state: State[S, A]) {
      def bind[B](f: A => State[S, B]): State[S, B] =
        monadState.bind(state)(f)
    }

    def relabel[A](t: Tree[A]): State[Int, Tree[(Int, A)]] =
      t match {
        case Leaf(x) =>
          i => (i + 1, Leaf((i, x)))

        case Node(l, r) =>
          relabel(l).bind(ll =>
            relabel(r).bind(rr =>
              point(Node(ll, rr))
            )
          )
      }

    assertEquals(
      relabel(
        Node(
          Leaf("a"),
          Node(Leaf("b"), Leaf("c"))
        )
      )(10),
      13 -> Node(
        Leaf(10 -> "a"),
        Node(Leaf(11 -> "b"), Leaf(12 -> "c"))
      )
    )
  }

  test("Validate with Monad") {
    val monadOption: Monad[Option] =
      new Monad[Option] {
        def point[A](a: A): Option[A] =
          a.some

        def bind[A, B](x: Option[A])(f: A => Option[B]): Option[B] =
          x.flatMap(f)
      }

    def point[A](a: A): Option[A] =
      monadOption.point(a)

    implicit class OptionExtension[A](x: Option[A]) {
      def bind[B](f: A => Option[B]): Option[B] =
        monadOption.bind(x)(f)
    }

    type Name = String
    type Age = Int

    final case class Person(name: Name, age: Age)

    val validateName: Name => Option[Name] =
      name => if (name.filter(_.isLetter) == name) name.some else none

    val validateAge: Age => Option[Age] =
      age => if (age > 0 && age < 110) age.some else none

    def validatePerson(s: String, n: Int): Option[Person] =
      validateName(s).bind(name =>
        validateAge(n).bind(age =>
          point(Person(name, age))
        )
      )

    assertEquals(validatePerson("Scooby", 5), Person("Scooby", 5).some)
    assertEquals(validatePerson("Scooby1", -5), none)
  }
}