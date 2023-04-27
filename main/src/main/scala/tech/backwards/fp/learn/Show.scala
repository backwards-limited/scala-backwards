package tech.backwards.fp.learn

import cats.implicits._
import tech.backwards.fp.learn.Show.syntax.ShowSyntax

trait Show[A] {
  def show(x: A): String
}

object Show { self =>
  def apply[A: Show]: Show[A] =
    implicitly

  def show[A: Show](x: A): String =
    apply[A].show(x)

  object syntax {
    implicit class ShowSyntax[A: Show](x: A) {
      lazy val show: String =
        self.show(x)
    }
  }

  implicit val showInt: Show[Int] =
    _.toString

  implicit val showDouble: Show[Double] =
    _.toString

  implicit def showList[A: Show]: Show[List[A]] =
    _.map(_.show).mkString("[", ", ", "]")
}