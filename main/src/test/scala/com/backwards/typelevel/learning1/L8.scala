package com.backwards.typelevel.learning1

import shapeless._

/**
 * Reach HList encoder.
 */
object L8 extends App {

  trait Encoder[A] {
    def encode(value: A): List[String]
  }

  object Encoder {
    def apply[T: Encoder](implicit dummyImplicit: DummyImplicit): Encoder[T] =
      implicitly[Encoder[T]]

    def apply[T](f: T => List[String]): Encoder[T] =
      t => f(t)
  }

  implicit def pairEncoder[A: Encoder, B: Encoder]: Encoder[(A, B)] =
    Encoder[(A, B)] { case (a, b) =>
      Encoder[A].encode(a) ::: Encoder[B].encode(b)
    }

  implicit def listEncoder[T: Encoder]: Encoder[List[T]] =
    Encoder[List[T]](_ flatMap Encoder[T].encode)

  implicit val encodeHNil: Encoder[HNil] =
    Encoder[HNil](_ => Nil)

  implicit def encodeHList[H: Encoder, T <: HList: Encoder]: Encoder[H :: T] =
    Encoder[H :: T] { case h :: t =>
      Encoder[H].encode(h) ::: Encoder[T].encode(t)
    }

  implicit val encodeString: Encoder[String] =
    Encoder[String](_ :: Nil)

  implicit val encodeInt: Encoder[Int] =
    Encoder[Int](_.toString :: Nil)

  implicit val encodeBoolean: Encoder[Boolean] =
    Encoder[Boolean](_.toString :: Nil)

  def encode[T: Encoder](value: T): List[String] =
    Encoder[T].encode(value)

  println(encode(List("Hello" :: 42 :: true :: HNil) :: List((42, true)) :: HNil))
}