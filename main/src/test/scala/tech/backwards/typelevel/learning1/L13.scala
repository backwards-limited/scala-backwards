package tech.backwards.typelevel.learning1

import shapeless._
import tech.backwards.typelevel._

/**
 * Dependent case class encoding with Lazy trick
 */
object L13 extends App {

  trait Encoder[A] {
    def encode(a: A): List[String]
  }

  implicit val intEncoder: Encoder[Int] =
    _.toString :: Nil

  implicit val doubleEncoder: Encoder[Double] =
    _.toString :: Nil

  implicit val stringEncoder: Encoder[String] =
    _ :: Nil

  implicit val hnilEncoder: Encoder[HNil] =
    _ => Nil

  implicit def hlistEncoder[H, T <: HList](
    implicit hEncoder: Lazy[Encoder[H]], // Wrap head in Lazy
    tEncoder: Encoder[T]
  ): Encoder[H :: T] = {
    // Lazy head
    case h :: t => hEncoder.value.encode(h) ++ tEncoder.encode(t)
  }

  implicit val cnilEncoder: Encoder[CNil] =
    _ => throw new Exception("Unattainable!")

  implicit def coproductEncoder[H, T <: Coproduct](
    implicit hEncoder: Lazy[Encoder[H]], // Wrap head in Lazy
    tEncoder: Encoder[T]
  ): Encoder[H :+: T] = {
    // Lazy head
    case Inl(h) => hEncoder.value.encode(h)
    case Inr(t) => tEncoder.encode(t)
  }

  implicit def genericEncoder[A, R](
    implicit gen: Generic.Aux[A, R],
    rEncoder: Lazy[Encoder[R]] // Wrap head in Lazy
  ): Encoder[A] =
    a => rEncoder.value.encode(a.toRepr)

  // Bar has richer constructor than `Foo`.
  // Compiler can't deal such things
  // without Lazy trick
  case class Bar(baz: Int, qux: String)
  case class Foo(bar: Bar)

  // Works only with Lazy
  println(implicitly[Encoder[Foo]])
  println(implicitly[Encoder[Bar]])
}