package tech.backwards.fp.learn

import cats.implicits._
import tech.backwards.fp.learn.TotalOrder.syntax.TotalOrderSyntax

trait TotalOrder[A] {
  def less(x: A, y: A): Boolean
}

object TotalOrder { self =>
  def apply[A: TotalOrder]: TotalOrder[A] =
    implicitly

  def less[A: TotalOrder](x: A, y: A): Boolean =
    apply[A].less(x, y)

  object syntax {
    implicit class TotalOrderSyntax[A: TotalOrder](x: A) {
      val less: A => Boolean =
        self.less(x, _)
    }
  }

  implicit val totalOrderInt: TotalOrder[Int] =
    _ < _

  implicit val totalOrderString: TotalOrder[String] =
    _ < _

  implicit def totalOrderList[A: TotalOrder]: TotalOrder[List[A]] =
    _.zip(_).foldM(false) { case (outcome, (x, y)) =>
      Option.unless(x less y)(outcome)
    } getOrElse true
}