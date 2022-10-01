package tech.backwards.fp.learn.typeclass

import cats.implicits._

trait TotalOrder[A] {
  def less(x: A, y: A): Boolean
}

object TotalOrder extends TotalOrderImplicits {
  def apply[A: TotalOrder]: TotalOrder[A] = implicitly

  def less[A: TotalOrder](x: A, y: A): Boolean =
    TotalOrder[A].less(x, y)

  object syntax {
    implicit class TotalOrderSyntax[A: TotalOrder](x: A) {
      lazy val less: A => Boolean =
        TotalOrder[A].less(x, _)
    }
  }
}

trait TotalOrderImplicits {
  import tech.backwards.fp.learn.typeclass.TotalOrder.syntax._

  implicit val totalOrderInt: TotalOrder[Int] =
    _ < _

  implicit val totalOrderString: TotalOrder[String] =
    _ < _

  implicit def totalOrderList[A: TotalOrder]: TotalOrder[List[A]] =
    _.zip(_).foldM(false) { case (outcome, (x, y)) =>
      if (x less y) none else outcome.some
    } getOrElse true
}