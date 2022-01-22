package tech.backwards.fp.monoid

object SimpleMonoidOps {
  final case class Sum(value: Int) extends AnyVal

  implicit val sumMonoid: Monoid[Sum] =
    new Monoid[Sum] {
      override def mzero: Sum =
        Sum(0)

      override def mappend(lhs: Sum, rhs: Sum): Sum =
        Sum(lhs.value + rhs.value)
    }

  final case class Product(value: Int) extends AnyVal

  implicit val productMonoid: Monoid[Product] =
    new Monoid[Product] {
      override def mzero: Product =
        Product(1)

      override def mappend(lhs: Product, rhs: Product): Product =
        Product(lhs.value * rhs.value)
    }
}