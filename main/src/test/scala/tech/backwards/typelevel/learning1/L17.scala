package tech.backwards.typelevel.learning1

import shapeless._
import shapeless.labelled._

/**
 * Encoding case classes into rich json
 */
object L17 extends App {

  sealed trait JsonValue
  case class JsonObject(fields: List[(String, JsonValue)]) extends JsonValue
  case class JsonArray(items: List[JsonValue]) extends JsonValue
  case class JsonString(value: String) extends JsonValue
  case class JsonNumber(value: Double) extends JsonValue
  case class JsonBoolean(value: Boolean) extends JsonValue
  case object JsonNull extends JsonValue

  trait JsonEncoder[A] {
    def encode(a: A): JsonValue
  }

  object JsonEncoder {
    def apply[A: JsonEncoder]: JsonEncoder[A] =
      implicitly[JsonEncoder[A]]

    def create[A](f: A => JsonValue): JsonEncoder[A] =
      a => f(a)
  }

  implicit val booleanEncoder: JsonEncoder[Boolean] =
    JsonEncoder create JsonBoolean

  implicit val intEncoder: JsonEncoder[Int] =
    JsonEncoder.create(n => JsonNumber(n))

  implicit val doubleEncoder: JsonEncoder[Double] =
    JsonEncoder create JsonNumber

  implicit val stringEncoder: JsonEncoder[String] =
    JsonEncoder create JsonString

  implicit def listEncoder[A: JsonEncoder]: JsonEncoder[List[A]] =
    JsonEncoder.create(list => JsonArray(list.map(JsonEncoder[A].encode)))

  implicit def optionEncoder[A: JsonEncoder]: JsonEncoder[Option[A]] =
    JsonEncoder.create(_.map(JsonEncoder[A].encode) getOrElse JsonNull)

  trait JsonObjectEncoder[A] extends JsonEncoder[A] {
    def encode(a: A): JsonObject
  }

  object JsonObjectEncoder {
    def create[A](f: A => JsonObject): JsonObjectEncoder[A] =
      a => f(a)
  }

  implicit val hnilEncoder: JsonObjectEncoder[HNil] =
    JsonObjectEncoder.create(_ => JsonObject(Nil))

  implicit def hlistObjectEncoder[K <: Symbol, H, T <: HList](
    implicit witness: Witness.Aux[K],
    hEncoder: Lazy[JsonEncoder[H]],
    tEncoder: JsonObjectEncoder[T]
  ): JsonObjectEncoder[FieldType[K, H] :: T] =
    JsonObjectEncoder.create { hlist =>
      val head = hEncoder.value.encode(hlist.head)
      val tail = tEncoder.encode(hlist.tail)

      val fieldName: String = witness.value.name

      JsonObject((fieldName, head) :: tail.fields)
    }

  implicit def genericObjectEncoder[A, H <: HList](
    implicit gen: LabelledGeneric.Aux[A, H],
    hEncoder: Lazy[JsonObjectEncoder[H]]
  ): JsonEncoder[A] =
    JsonObjectEncoder.create(a =>
      hEncoder.value.encode(gen.to(a))
    )

  case class Document(path: String, size: Int, readOnly: Boolean)

  val document = Document("/", 42, readOnly = true)

  println(JsonEncoder[Document].encode(document))
}