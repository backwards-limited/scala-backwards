package tech.backwards.fp.learn.monoid

trait Monoid[A] {
  def mzero: A

  def mappend(x: A, y: A): A
}

object Monoid extends MonoidImplicits {
  def apply[A: Monoid]: Monoid[A] = implicitly

  object syntax {
    implicit class MonoidSyntax[A: Monoid](x: A) {
      val |+| : A => A =
        Monoid[A].mappend(x, _)
    }
  }
}

sealed trait MonoidImplicits {
  implicit val monoidSum: Monoid[Sum] =
    new Monoid[Sum] {
      def mzero: Sum =
        Sum(0)

      def mappend(x: Sum, y: Sum): Sum =
        Sum(x.value + y.value)
    }

  implicit val monoidProduct: Monoid[Product] =
    new Monoid[Product] {
      def mzero: Product =
        Product(1)

      def mappend(x: Product, y: Product): Product =
        Product(x.value * y.value)
    }
}