package tech.backwards.fp.typeclass

import shapeless.{::, Generic, HList, HNil, Lazy}

trait Sum[T] {
  def sum(a: T, b: T): T
}

object Sum {
  def apply[T: Sum]: Sum[T] =
    implicitly

  object ops {
    implicit class SumOps[T: Sum](a: T) {
      def |+|(b: T): T =
        Sum[T].sum(a, b)
    }
  }

  implicit val intSum: Sum[Int] =
    _ + _

  implicit val stringSum: Sum[String] =
    _ + _

  implicit def genericSum[T, R](implicit Gen: Generic.Aux[T, R], Sum: Lazy[Sum[R]]): Sum[T] =
    (a: T, b: T) =>
      Gen from Sum.value.sum(Gen to a, Gen to b)

  implicit val hnilSum: Sum[HNil] =
    (a: HNil, b: HNil) => a

  implicit def hconsSum[H, T <: HList](implicit SumH: Lazy[Sum[H]], SumT: Lazy[Sum[T]]): Sum[H :: T] =
    (a: H :: T, b: H :: T) =>
      SumH.value.sum(a.head, b.head) :: SumT.value.sum(a.tail, b.tail)
}

trait SumDifferentTypes[T, U] {
  type Res

  def sum(a: T, b: U): Res
}

object SumDifferentTypes {
  object ops {
    implicit class SumDifferentTypesFunctionsOps[A, B, C](a: A => B)(implicit SumDifferentTypes: SumDifferentTypes[A => B, B => C] { type Res = A => C }) {
      def |+|(b: B => C): A => C =
        SumDifferentTypes.sum(a, b)
    }

    implicit class SumDifferentTypesOps[A, B, C](a: A)(implicit SumDifferentTypes: SumDifferentTypes[A, B] { type Res = C }) {
      def |+|(b: B): C =
        SumDifferentTypes.sum(a, b)
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