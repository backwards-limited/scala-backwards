package com.backwards.fp.typeclass

import scala.concurrent.ExecutionContext
import scala.language.postfixOps
import shapeless.{:+:, ::, CNil, Coproduct, Generic, HList, HNil, Inl, Inr, Lazy}

trait Eq[T] {
  def eq(a: T, b: T): Boolean
}

object Eq {
  def apply[T: Eq]: Eq[T] = implicitly

  object ops {
    implicit class EqOps[T: Eq](a: T) {
      def |=|(b: T): Boolean = Eq[T].eq(a, b)
    }
  }

  implicit val intEq: Eq[Int] = _ == _

  implicit val stringEq: Eq[String] = _ == _

  implicit val booleanEq: Eq[Boolean] = _ == _

  // This is not production code!
  import scala.concurrent.Future

  implicit def futureEq[T](implicit eq: Lazy[Eq[T]], ec: ExecutionContext): Eq[Future[T]] = {
    import scala.concurrent.Await
    import scala.concurrent.duration._

    (aFuture: Future[T], bFuture: Future[T]) =>
      Await.result(
        for {
          a <- aFuture
          b <- bFuture
        } yield eq.value.eq(a, b),
      100 millis)
  }

  implicit def genericEq[T, R](implicit G: Generic.Aux[T, R], eq: Lazy[Eq[R]]): Eq[T] =
    (a: T, b: T) => eq.value.eq(G.to(a), G.to(b))

  implicit val hnilEq: Eq[HNil] =
    (a: HNil, b: HNil) => true

  implicit def hconsEq[H, T <: HList](implicit EH: Lazy[Eq[H]], ET: Lazy[Eq[T]]): Eq[H :: T] =
    (a: H :: T, b: H :: T) =>
      EH.value.eq(a.head, b.head) && ET.value.eq(a.tail, b.tail)

  implicit val cnilEq: Eq[CNil] =
    (a: CNil, b: CNil) => true

  implicit def cconsEq[H, T <: Coproduct](implicit EH: Lazy[Eq[H]], ET: Lazy[Eq[T]]): Eq[H :+: T] =
    (a: H :+: T, b: H :+: T) => (a, b) match {
      case (Inl(ah), Inl(bh)) => EH.value.eq(ah, bh)
      case (Inr(at), Inr(bt)) => ET.value.eq(at, bt)
      case _ => false
    }
}