package com.backwards.adhoc

import _root_.cats.{Id, Monad}
import _root_.cats.implicits._
import org.scalatest.matchers.must.Matchers
import org.scalatest.wordspec.AnyWordSpec

/**
 * Define a data type D that represents a document subdivided horizontally or vertically into 1 or more cells,
 * that in turn can be further subdivided or can hold a value of some type A (A is a parameter).
 * Equip the new data type D with:
 * {{{
 *  f[M[_]: Monad, A, B]: (A => M[B]) => D[A] => M[D[B]]
 * }}}
 *
 * such that
 * {{{
 *  f[Id](identity) = identity
 *  f[Option](Some(_)) = Some(_)
 * }}}
 *
 * where Id[A] = A
 */
class SambaSpec extends AnyWordSpec with Matchers {
  sealed trait D[T]

  object D {
    def apply[M[_]: Monad, A, B](fn: A => M[B])(d: D[A]): M[D[B]] =
      f[M, A, B].andThen(_(d))(fn)

    def f[M[_]: Monad, A, B]: (A => M[B]) => D[A] => M[D[B]] =
      fn => {
        case Cell(a) =>
          fn(a).map(Cell.apply)

        case Cells(ds) =>
          ds.foldRight(List.empty[D[B]].pure[M]) { case (d, cells) =>
            cells.flatMap { list =>
              apply(fn)(d).map(_ +: list)
            }
          }.map(Cells.apply)
      }
  }

  final case class Cell[A](a: A) extends D[A]

  final case class Cells[A](ds: List[D[A]]) extends D[A]

  "Document" should {
    "be monadically manipulated within Id" in {
      val int2String: Int => Id[String] =
        i => s"Index: $i"

      D(int2String)(Cell(5)) mustBe Cell("Index: 5")

      D(int2String)(Cells(List())) mustBe Cells(List())
      D(int2String)(Cells(List(Cell(5), Cell(6), Cells(List(Cell(17)))))) mustBe Cells(List(Cell("Index: 5"), Cell("Index: 6"), Cells(List(Cell("Index: 17")))))
    }

    "be monadically manipulated within Option" in {
      val int2String: Int => Option[String] =
        i => Option(s"Index: $i")

      D(int2String)(Cell(5)) mustBe Option(Cell("Index: 5"))

      D(int2String)(Cells(List())) mustBe Option(Cells(List()))
      D(int2String)(Cells(List(Cell(5), Cell(6), Cells(List(Cell(17)))))) mustBe Option(Cells(List(Cell("Index: 5"), Cell("Index: 6"), Cells(List(Cell("Index: 17"))))))
    }
  }
}