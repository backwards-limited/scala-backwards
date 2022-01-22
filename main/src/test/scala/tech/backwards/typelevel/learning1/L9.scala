package tech.backwards.typelevel.learning1

import shapeless._

/**
 * Generic case class encoder with ability to encode HList of case classes (see Lazy trick).
 */
object L9 extends App {

  trait Encoder[A] {
    def encode(value: A): List[String]
  }

  object Encoder {
    def apply[T: Encoder](implicit dummyImplicit: DummyImplicit): Encoder[T] =
      implicitly[Encoder[T]]

    def apply[T](f: T => List[String]): Encoder[T] =
      t => f(t)
  }

  case class Employee(name: String, number: Int, manager: Boolean)

  implicit def pairEncoder[A: Encoder, B: Encoder]: Encoder[(A, B)] =
    Encoder[(A, B)] { case (a, b) =>
      Encoder[A].encode(a) ::: Encoder[B].encode(b)
    }

  implicit def listEncoder[T: Encoder]: Encoder[List[T]] =
    Encoder[List[T]](_ flatMap Encoder[T].encode)

  implicit val encodeHNil: Encoder[HNil] =
    Encoder[HNil](_ => Nil)

  implicit def encodeHList[H, T <: HList](
    implicit hEncoder: Lazy[Encoder[H]], // trick
    tEncoder: Encoder[T]
  ): Encoder[H :: T] =
    Encoder[H :: T] { case h :: t =>
      hEncoder.value.encode(h) ::: tEncoder.encode(t)
    }

  implicit val encodeString: Encoder[String] =
    Encoder[String](_ :: Nil)

  implicit val encodeInt: Encoder[Int] =
    Encoder[Int](_.toString :: Nil)

  implicit val encodeBoolean: Encoder[Boolean] =
    Encoder[Boolean](_.toString :: Nil)

  implicit def genericEncoder[A, R <: HList](
    implicit generic: Generic.Aux[A, R],
    encoder: Encoder[R]
  ): Encoder[A] =
    Encoder[A](a => encoder.encode(generic.to(a)))

  def encode[T: Encoder](value: T): List[String] =
    Encoder[T].encode(value)

  println(encode(Employee("John", 42, manager = true)))
  println(encode(Employee("John", 42, manager = true) :: HNil))
}