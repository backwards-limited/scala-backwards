package com.backwards.fp.typeclass

import shapeless.{::, Generic, HList, HNil, Lazy}

trait Sum[T] {
  def sum(a: T, b: T): T
}

object Sum {
  def apply[T: Sum]: Sum[T] = implicitly

  object ops {
    implicit class SumOps[T: Sum](a: T) {
      def |+|(b: T): T = Sum[T].sum(a, b)
    }
  }

  implicit val intSum: Sum[Int] = _ + _

  implicit val stringSum: Sum[String] = _ + _

  implicit def genericSum[T, R](implicit gen: Generic.Aux[T, R], sum: Lazy[Sum[R]]): Sum[T] =
    (a: T, b: T) =>
      gen from sum.value.sum(gen to a, gen to b)

  implicit val hnilSum: Sum[HNil] =
    (a: HNil, b: HNil) => a

  implicit def hconsSum[H, T <: HList](implicit hSum: Lazy[Sum[H]], tSum: Lazy[Sum[T]]): Sum[H :: T] =
    (a: H :: T, b: H :: T) =>
      hSum.value.sum(a.head, b.head) :: tSum.value.sum(a.tail, b.tail)
}

trait SumDifferentTypes[T, U] {
  type Res

  def sum(a: T, b: U): Res
}

object SumDifferentTypes {
  object ops {
    implicit class SumDifferentTypesFunctionsOps[A, B, C](a: A => B)(implicit sdt: SumDifferentTypes[A => B, B => C] { type Res = A => C }) {
      def |+|(b: B => C): A => C = sdt.sum(a, b)
    }

    implicit class SumDifferentTypesOps[A, B, C](a: A)(implicit sdt: SumDifferentTypes[A, B] { type Res = C }) {
      def |+|(b: B): C = sdt.sum(a, b)
    }
  }

  implicit val intStringBooleanSum: SumDifferentTypes[Int, String] { type Res = Boolean } = new SumDifferentTypes[Int, String] {
    type Res = Boolean

    def sum(a: Int, b: String): Boolean =
      b.length == a
  }

  implicit def functionSum[A, B, C]: SumDifferentTypes[A => B, B => C] { type Res = A => C } = new SumDifferentTypes[A => B, B => C] {
    type Res = A => C

    def sum(a: A => B, b: B => C): A => C =
      a andThen b
  }
}