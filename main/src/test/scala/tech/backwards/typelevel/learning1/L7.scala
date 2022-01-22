package tech.backwards.typelevel.learning1

/**
 * Reach employee encoder.
 */
object L7 extends App {

  trait Encoder[A] {
    def encode(value: A): List[String]
  }

  object Encoder {
    def apply[A: Encoder]: Encoder[A] = implicitly[Encoder[A]]
  }

  case class Employee(name: String, number: Int, manager: Boolean)

  implicit val employeeEncoder: Encoder[Employee] =
    emp => emp.name :: emp.number.toString :: emp.manager.toString :: Nil

  implicit def pairEncoder[A: Encoder, B: Encoder]: Encoder[(A, B)] = {
    case (a, b) => Encoder[A].encode(a) ::: Encoder[B].encode(b)
  }

  implicit def listEncoder[T: Encoder]: Encoder[List[T]] =
    _ flatMap Encoder[T].encode

  def encode[T: Encoder](value: T): List[String] =
    Encoder[T] encode value

  val alice = Employee("Alice", 42, manager = true)
  val bob = Employee("Bob", 19, manager = false)
  val dave = Employee("Dave", 17, manager = false)
  val anna = Employee("Anna", 43, manager = false)

  val list = List((alice, bob), (dave, anna))

  println(encode(list))

}