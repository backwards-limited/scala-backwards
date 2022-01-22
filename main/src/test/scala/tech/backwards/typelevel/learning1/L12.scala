package tech.backwards.typelevel.learning1

import shapeless._
import tech.backwards.typelevel._

/**
 * Sealed trait hierarchy encoding with Lazy trick
 */
object L12 extends App {

  sealed trait Shape

  final case class Rectangle(width: Double, height: Double) extends Shape

  final case class Circle(radius: Double) extends Shape

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

  val rect: Shape = Rectangle(1.0, 2.0)
  val circ: Shape = Circle(3.0)

  println(implicitly[Encoder[Shape]].encode(rect))
  println(implicitly[Encoder[Shape]].encode(circ))
}