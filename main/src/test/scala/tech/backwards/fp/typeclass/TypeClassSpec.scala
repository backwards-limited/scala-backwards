package tech.backwards.fp.typeclass

import shapeless._
import shapeless.ops.{hlist, nat}
import org.scalatest.matchers.must.Matchers
import org.scalatest.wordspec.AnyWordSpec

class TypeClassSpec extends AnyWordSpec with Matchers {
  "Typeclass" should {
    "apply the int instance" in {
      import tech.backwards.fp.typeclass.TypeClass._
      import tech.backwards.fp.typeclass.TypeClassInstances._

      foo(5) mustEqual "int"
    }

    "apply the string instance" in {
      import tech.backwards.fp.typeclass.TypeClass._
      import tech.backwards.fp.typeclass.TypeClassInstances._

      foo("5") mustEqual "string"
    }
  }

  "Typeclass 'has a' relationship" should {
    "use trait to define behaviour, then parametric function that uses composition (instead of inheritance)" in {
      trait Random[A] {
        def get: A
      }

      def random[A: Random]: A = implicitly[Random[A]].get

      // Instance constructor:
      def createRandom[A](func: () => A): Random[A] =
        new Random[A] {
          def get: A = func()
        }

      // Random numbers from 0 to 9:
      implicit val intRandom: Random[Int] =
        createRandom(() => scala.util.Random.nextInt(10))

      // Random characters from A to Z:
      implicit val charRandom: Random[Char] =
        createRandom(() => ('A'.toInt + scala.util.Random.nextInt(26)).toChar)

      // Random booleans:
      implicit val booleanRandom: Random[Boolean] =
        createRandom(() => scala.util.Random.nextBoolean())

      for (_ <- 1 to 3) println(random[Int])
      // 0
      // 8
      // 9

      for (_ <- 1 to 3) println(random[Char])
      // V
      // N
      // J
    }

    "use trait to define behaviour, then parametric function that uses composition (instead of inheritance) with some shapeless" in {
      trait SizeOf[A] {
        def value: Int
      }

      def sizeOf[A: SizeOf]: Int = implicitly[SizeOf[A]].value

      // To create an instance of SizeOf we need three things:
      // 1. a Generic to calculate the corresponding HList type
      // 2. a Length to calculate the length of the HList as a Nat
      // 3. a ToInt to convert the Nat to anInt

      implicit def genericSizeOf[A, L <: HList, N <: Nat](
        implicit generic: Generic.Aux[A, L],
        size: hlist.Length.Aux[L, N],
        sizeToInt: nat.ToInt[N]
      ): SizeOf[A] =
        new SizeOf[A] {
          val value: Int = sizeToInt.apply()
        }

      case class IceCream(name: String, numCherries: Int, inCone: Boolean)

      sizeOf[IceCream]
      // res3: Int = 3
    }
  }
}