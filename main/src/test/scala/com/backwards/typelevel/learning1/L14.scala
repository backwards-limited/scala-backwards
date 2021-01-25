package com.backwards.typelevel.learning1

import shapeless._
import com.backwards.typelevel._

/**
 * Recursive structures encoding with Lazy trick
 */
object L14 extends App {
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

  // `Tree` is a recursive structure
  // Compiler can't deal such things
  // without Lazy trick
  sealed trait Tree[A]

  case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

  case class Leaf[A](value: A) extends Tree[A]

  println(implicitly[Encoder[Leaf[Int]]])
}