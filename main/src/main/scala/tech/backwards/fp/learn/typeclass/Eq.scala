package tech.backwards.fp.learn.typeclass

import cats.implicits._

trait Eq[A] {
  def eq(x: A, y: A): Boolean
}

object Eq extends EqImplicits { self =>
  def apply[A: Eq]: Eq[A] = implicitly

  def eq[A: Eq](x: A, y: A): Boolean =
    apply[A].eq(x, y)

  object syntax {
    implicit class EqSyntax[A: Eq](x: A) {
      val ==== : A => Boolean =
        self.eq(x, _)

      val !=== : A => Boolean =
        ==== andThen (!_)
    }
  }
}

sealed trait EqImplicits {
  import tech.backwards.fp.learn.typeclass.Eq.syntax._

  implicit val eqInt: Eq[Int] =
    _ == _

  implicit val eqString: Eq[String] =
    _ == _

  /*
  Obvious version - Recursion to short circuit
  implicit def eqList[A: Eq]: Eq[List[A]] = {
    lazy val eq: (List[A], List[A]) => Boolean = {
      case (Nil, Nil) => true
      case (x :: xs, y :: ys) if x ==== y => eq(xs, ys)
      case _ => false
    }

    eq(_, _)
  }*/

  // Alternative version - foldM to short circuit
  implicit def eqList[A: Eq]: Eq[List[A]] =
    (xs, ys) =>
      (xs.length == ys.length) && xs.zip(ys).foldM(true) { case (outcome, (x, y)) =>
        Option.when(x ==== y)(outcome)
      }.getOrElse(false)
}