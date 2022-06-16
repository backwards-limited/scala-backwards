package tech.backwards.bookofmonads.ch1

import munit._

/**
 * Lists
 */
class Ex2Suite extends FunSuite {
  test("List length") {
    def length[A](xs: List[A]) : Int =
      xs match {
        case Nil => 0
        case _ :: xs => 1 + length(xs)
      }

    assertEquals(length(Nil), 0)
    assertEquals(length(List("a", "b", "c")), 3)
  }

  test("List append") {
    def append[A](xs: List[A], ys: List[A]): List[A] =
      xs match {
        case Nil => ys
        case x :: xs => x :: append(xs, ys)
      }

    assertEquals(
      append(List("a", "b"), List("c", "d")),
      List("a", "b", "c", "d")
    )
  }

  test("Map over a list") {
    def map[A, B](f: A => B, xs: List[A]): List[B] =
      xs match {
        case x :: xs => f(x) :: map(f, xs)
        case _ => Nil
      }

    assertEquals(
      map((s: String) => s + "*", List("a", "b")),
      List("a*", "b*")
    )

    assertEquals(
      map((s: String) => s + "*", Nil),
      Nil
    )
  }

  test("Singleton list") {
    def singleton[A](x: A): List[A] =
      x :: Nil

    assertEquals(
      singleton("a"),
      List("a")
    )
  }

  test("Concatenate list") {
    // This operation is called flatten in the built-in List class
    def concat[A](xs: List[List[A]]): List[A] =
      xs match {
        case Nil => Nil
        case x :: xs => x ++ concat(xs)
      }

    assertEquals(
      concat(List(List("a", "b"), List("c", "d"))),
      List("a", "b", "c", "d")
    )
  }
}