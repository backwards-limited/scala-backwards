package tech.backwards.fp.typeclass

trait Order[A] {
  def less(lhs:A, rhs:A): Boolean
}

object Order {
  @annotation.implicitNotFound("No instance of Order found")
  def less[A](lhs: A, rhs: A)(implicit Order: Order[A]): Boolean =
    Order.less(lhs, rhs)
}

object OrderInstances {
  implicit val intInstance: Order[Int] =
    (lhs: Int, rhs: Int) => lhs < rhs

  implicit val stringInstance: Order[String] =
    (lhs: String, rhs: String) => lhs < rhs

  implicit def listInstance[T](implicit Order: Order[T]): Order[List[T]] =
    (lhs: List[T], rhs: List[T]) => lhs zip rhs forall {
      case (l, r) => Order.less(l, r)
    }
}