package tech.backwards.fp.learn

trait Monoid[A] {
  def mzero: A

  def mappend(x: A, y: A): A
}

object Monoid extends MonoidImplicits {
  def apply[A: Monoid]: Monoid[A] =
    implicitly

  object syntax {
    implicit class MonoidSyntax[A: Monoid](x: A) {
      val |+| : A => A =
        apply[A].mappend(x, _)
    }
  }
}

sealed trait MonoidImplicits {
  import tech.backwards.fp.learn.Monoid.syntax._

  implicit val monoidSum: Monoid[Sum] =
    new Monoid[Sum] {
      lazy val mzero: Sum =
        Sum(0)

      def mappend(x: Sum, y: Sum): Sum =
        Sum(x.value + y.value)
    }

  implicit val monoidProduct: Monoid[Product] =
    new Monoid[Product] {
      lazy val mzero: Product =
        Product(1)

      def mappend(x: Product, y: Product): Product =
        Product(x.value * y.value)
    }

  implicit def monoidList[A]: Monoid[List[A]] =
    new Monoid[List[A]] {
      lazy val mzero: List[A] =
        Nil

      def mappend(xs: List[A], ys: List[A]): List[A] =
        xs ++ ys
    }

  implicit def monoidPair[A: Monoid, B: Monoid]: Monoid[Pair[A, B]] =
    new Monoid[Pair[A, B]] {
      lazy val mzero: Pair[A, B] =
        Pair(Monoid[A].mzero, Monoid[B].mzero)

      def mappend(x: Pair[A, B], y: Pair[A, B]): Pair[A, B] =
        (x, y) match {
          case (Pair(xA, xB), Pair(yA, yB)) =>
            Pair(xA |+| yA, xB |+| yB)
        }
    }
}