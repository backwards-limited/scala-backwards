package tech.backwards.fp.learn

import cats.implicits._

trait Show[A] {
  def show(x: A): String
}

object Show extends ShowImplicits { self =>
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
}

sealed trait ShowImplicits {
  import tech.backwards.fp.learn.Show.syntax._

  implicit val showInt: Show[Int] =
    _.toString

  implicit val showDouble: Show[Double] =
    _.toString

  implicit def showList[A: Show]: Show[List[A]] =
    _.map(_.show).mkString("[", ", ", "]")
}