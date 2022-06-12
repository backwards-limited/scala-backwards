package tech.backwards.bookofmonads.ch0

import scala.annotation.tailrec
import munit._
import org.scalacheck.Prop._

/**
 * When using the type class pattern, the declaration of which functionality each type should provide is given in the form of a trait.
 * Such a trait always takes a type parameter, which represents the type we will be working with.
 */
class Ex2Suite extends FunSuite with Ex2Fixture {
  test("Equality of integers") {
    assertEquals(Eq[Int].eq(5, 5), true)
    assertNotEquals(Eq[Int].notEq(5, 5), true)
    assertNotEquals(Eq[Int].eq(5, 4), true)
    assertEquals(Eq[Int].notEq(5, 4), true)
  }

  test("Equality of integers via function") {
    def eq[A: Eq](x: A, y: A): Boolean =
      implicitly[Eq[A]].eq(x, y)

    assertEquals(eq(5, 5), true)
    assertNotEquals(eq(5, 4), true)
  }

  test("Equality of integer list") {
    def eq[A: Eq](xs: List[A], ys: List[A]): Boolean =
      Eq[List[A]].eq(xs, ys)

    def notEq[A: Eq](xs: List[A], ys: List[A]): Boolean =
      Eq[List[A]].notEq(xs, ys)

    assertEquals(eq(List(1, 2, 3), List(1, 2, 3)), true)
    assertNotEquals(notEq(List(1, 2, 3), List(1, 2, 3)), true)
    assertNotEquals(eq(List(1, 2, 3), List(4, 5, 6)), true)
    assertEquals(notEq(List(1, 2, 3), List(4, 5, 6)), true)
  }

  test("Equality of tuples") {
    assertEquals(Eq[(Int, Int)].eq(5 -> 6, 5 -> 6), true)
  }
}

class Ex2ScalaCheckSuite extends ScalaCheckSuite with Ex2Fixture {
  property("Equality of integers") {
    forAll { (n: Int) =>
      assertEquals(Eq[Int].eq(n, n), true)
    }
  }
}

trait Ex2Fixture {
  trait Eq[A] {
    def eq(x: A, y: A): Boolean

    def notEq(x: A, y: A): Boolean
  }

  object Eq {
    def apply[A: Eq]: Eq[A] =
      implicitly // Or more directly: implicitly[Eq[A]]
  }

  implicit val eqInt: Eq[Int] =
    new Eq[Int] {
      def eq(x: Int, y: Int): Boolean =
        x == y

      def notEq(x: Int, y: Int): Boolean =
        !eq(x, y)
    }

  implicit def eqList[A: Eq]: Eq[List[A]] =
    new Eq[List[A]] {
      @tailrec
      def eq(xs: List[A], ys: List[A]): Boolean =
        (xs, ys) match {
          case (x :: xs, y :: ys) =>
            if (Eq[A].eq(x, y)) eq(xs, ys) else false
          case (xs, ys) =>
            xs == Nil && ys == Nil
        }

      def notEq(xs: List[A], ys: List[A]): Boolean =
        !eq(xs, ys)
    }

  implicit def eqTuple[A: Eq]: Eq[(A, A)] =
    new Eq[(A, A)] {
      def eq(x: (A, A), y: (A, A)): Boolean =
        Eq[A].eq(x._1, y._1) && Eq[A].eq(x._2, y._2)

      def notEq(x: (A, A), y: (A, A)): Boolean =
        !eq(x, y)
    }
}