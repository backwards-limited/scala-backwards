package com.backwards.variance

import scala.util.matching.Regex
import org.scalatest.matchers.must.Matchers
import org.scalatest.wordspec.AnyWordSpec

/**
  * Checking that a snippet of code does not compile
  * Often when creating libraries you may wish to ensure that certain arrangements of code that represent potential “user errors” do not compile, so that your library is more error resistant. ScalaTest Matchers trait includes the following syntax for that purpose:
  *
  * "val a: String = 1" mustNot compile
  * If you want to ensure that a snippet of code does not compile because of a type error (as opposed to a syntax error), use:
  *
  * "val a: String = 1" mustNot typeCheck
  * Note that the shouldNot typeCheck syntax will only succeed if the given snippet of code does not compile because of a type error. A syntax error will still result on a thrown TestFailedException.
  *
  * If you want to state that a snippet of code does compile, you can make that more obvious with:
  *
  * "val a: Int = 1" must compile
  * Although the previous three constructs are implemented with macros that determine at compile time whether the snippet of code represented by the string does or does not compile, errors are reported as test failures at runtime.
  *
  * Summary:
  *
  *   - F[+A] makes F covariant on A. This means that if B <:< A (where B extends A), then F[B] <:< F[A]
  *   - F[-A] makes F contravariant on A. If B <:< A, then F[A]  <:< F[B]
  *   - F[A] makes F invariant on A. If B <:< A, then there is no relationship between F[A] and F[B]
  *   - If you declare a type parameter as covariant with +A, then the A type can only appear in the result of a method
  *   - If you declare a type parameter as contravariant with -A, then the A type can only appear in the parameters of a method
  */
class VarianceSpec extends AnyWordSpec with Matchers {
  class CSuper

  class C extends CSuper

  class CSub extends C
  class C1Sub extends C

  "Variance" should {
    "demo" in {
      def func(f: C => C) = f(new C)

      val f: C1Sub => C =
        c => c

      "func(f)" mustNot compile
    }

    "show invariance" in {
      sealed trait Glass[Contents]

      case class Full[Contents](contents: Contents) extends Glass[Contents]

      case object Empty extends Glass[Nothing]


      case class Water(purity: Int)

      def drink(glass: Glass[Water]): Unit = ()

      // We can drink from the full glass but not from an empty one:
      drink(Full(Water(100)))

      "drink(Empty)" mustNot compile
      /*
      The compilation error would be similar to:
      type mismatch;
      found   : Empty.type
      required: Glass[Water]
      Note: Nothing <: Water (and Empty.type <: Glass[Nothing]), but trait Glass is invariant in type Contents.
      You may wish to define Contents as +Contents instead. (SLS 4.5)
      drink(Empty) // compile error, wrong type of contents
      */

      // What if instead of drinking, we'd like to define drinkAndRefill, which should refill an empty glass?

      def drinkAndRefill(glass: Glass[Water]): Unit = ()

      // Obviously the following will still not compile because drinkAndRefill is invariant in Contents as with drink
      "drinkAndRefill(Empty)" mustNot compile

      /*
      We would like our implementation to accept not only Glass[Water], but also Glass[Nothing],
      or more generally any Glass[B] if B <: Water.
      We can change our implementation accordingly, but the following is bad as we would have to change all functions that accept Glass[B]:
      */
      def drinkAndRefillBad[B <: Water](glass: Glass[B]): Unit = ()

      drinkAndRefillBad(Empty)

      /*
      The first definition of sealed trait Glass[Contents], is called invariant,
      and it means that the relation in the type that parameterizes Glass does not affect how glasses with different contents are related—they are not related at all.
      Covariance means that, in regards to the compiler, if type parameters are in a subclass relation, then the main types should be too.
      */
    }

    "show covariance" in {
      sealed trait Glass[+Contents] // Note the +

      case class Full[Contents](contents: Contents) extends Glass[Contents]

      case object Empty extends Glass[Nothing]


      case class Water(purity: Int)

      def drink(glass: Glass[Water]): Unit = ()

      drink(Empty) // Now compiles, unlike the invariance example

      /*
      A typical use of covariance is with different kinds of immutable containers,
      where it is safe to have a more specific element in the container, like the one that is declared by the type.
      It is not safe to do so with mutable containers, though.
      */
    }

    "show contravariance" in {
      class Drinker[T] {
        def drink(contents: T): Unit = ()
      }

      sealed trait Glass[Contents] {
        def contents: Contents

        def knockBack(drinker: Drinker[Contents]): Unit =
          drinker.drink(contents)
      }

      case class Full[C](contents: C) extends Glass[C]


      class Water(purity: Int)

      class PureWater(purity: Int) extends Water(purity) {
        def shine: Unit = ()
      }

      val glass = Full(new PureWater(100))
      glass.knockBack(new Drinker[PureWater])


      // If somebody can drink just water, they should be able to drink pure water as well:
      "glass.knockBack(new Drinker[Water])" mustNot compile
      /*
      type mismatch;
      found   : Drinker[Water]
      required: Drinker[PureWater]
      Note: Water >: PureWater, but class Drinker is invariant in type T.
      You may wish to define T as -T instead. (SLS 4.5)
      glass.knockBack(new Drinker[Water]) // compile error
      */
    }

    "fix with contravariance" in {
      class Drinker[-T] { // Note the -
        def drink(contents: T): Unit = ()
      }

      sealed trait Glass[Contents] {
        def contents: Contents

        def knockBack(drinker: Drinker[Contents]): Unit =
          drinker.drink(contents)
      }

      case class Full[C](contents: C) extends Glass[C]


      class Water(purity: Int)

      class PureWater(purity: Int) extends Water(purity) {
        def shine: Unit = ()
      }

      val glass = Full(new PureWater(100))
      glass.knockBack(new Drinker[PureWater])


      // If somebody can drink just water, they should be able to drink pure water as well:
      glass.knockBack(new Drinker[Water])
    }
  }

  "Invariance" should {
    "show example" in {
      trait Animal

      case class Cat(name: String) extends Animal
      case class Dog(name: String) extends Animal

      val animal1: Animal = Cat("Max")
      val animal2: Animal = Dog("Dolly")

      // You can check that type B extends type A with the expression implicitly[B <:< A]; if it compiles, then B is a subtype of A.
      implicitly[Dog <:< Animal]


      // InvariantDecoder is invariant on A:
      trait InvariantDecoder[A] {
        def decode(s: String): Option[A]
      }

      // Implement for Cat
      object CatInvariantDecoder extends InvariantDecoder[Cat] {
        val CatRegex: Regex = """Cat\((\w+)\)""".r

        def decode(s: String): Option[Cat] = s match {
          case CatRegex(name) => Option(Cat(name))
          case _ => None
        }
      }

      CatInvariantDecoder.decode("Cat(Max)") mustBe Option(Cat("Max"))

      "val animalInvariantDecoder: InvariantDecoder[Animal] = CatInvariantDecoder" mustNot compile
      /*
      Compile error:
      type mismatch;
      found   : CatInvariantDecoder.type
      required: InvariantDecoder[Animal]
      Note: Cat <: Animal (and CatInvariantDecoder.type <: InvariantDecoder[Cat]), but trait InvariantDecoder is invariant in type A.
      You may wish to define A as +A instead. (SLS 4.5)
      val invariantAnimalDecoder: InvariantDecoder[Animal] = CatInvariantDecoder
      */
    }
  }

  "Covariance" should {
    "show example - fixing the issue we had with invariance" in {
      trait Animal

      case class Cat(name: String) extends Animal
      case class Dog(name: String) extends Animal

      val animal1: Animal = Cat("Max")
      val animal2: Animal = Dog("Dolly")

      // You can check that type B extends type A with the expression implicitly[B <:< A]; if it compiles, then B is a subtype of A.
      implicitly[Dog <:< Animal]


      // CovariantDecoder is covariant on A:
      trait CovariantDecoder[+A] {
        def decode(s: String): Option[A]
      }

      // Implement for Cat
      object CatCovariantDecoder extends CovariantDecoder[Cat] {
        val CatRegex: Regex = """Cat\((\w+)\)""".r

        def decode(s: String): Option[Cat] = s match {
          case CatRegex(name) => Option(Cat(name))
          case _ => None
        }
      }

      CatCovariantDecoder.decode("Cat(Max)") mustBe Option(Cat("Max"))

      // Compiles, whereas the invariant version did not
      val animalCovariantDecoder: CovariantDecoder[Animal] = CatCovariantDecoder

      // Proof of compilation:
      implicitly[CovariantDecoder[Cat] <:< CovariantDecoder[Animal]]
    }
  }

  "Contravariance" should {
    "show example" in {
      trait Animal

      case class Cat(name: String) extends Animal
      case class Dog(name: String) extends Animal

      val animal1: Animal = Cat("Max")
      val animal2: Animal = Dog("Dolly")

      // You can check that type B extends type A with the expression implicitly[B <:< A]; if it compiles, then B is a subtype of A.
      implicitly[Dog <:< Animal]


      trait Encoder[-A] {
        def encode(a: A): String
      }

      object AnimalEncoder extends Encoder[Animal] {
        def encode(a: Animal): String = a.toString
      }

      /*
      We have the Cat <:< Animal relationship, and Encoder is contravariant on its argument.
      This implies that Encoder[Animal] <:< Encoder[Cat], and I can, therefore, assign Encoder[Animal] to a variable of type Encoder[Cat]
      */
      val catEncoder: Encoder[Cat] = AnimalEncoder
      catEncoder.encode(Cat("Luna")) mustBe "Cat(Luna)"

      // What if we wanted a Codec for both Encoder and Decoder
      // Now we have an issue because a parameterised type cannot be both covariant and contravariant:

      """
      trait Codec[+A] {
        def encode(a: A): String
        def decode(s: String): Option[A]
      }
      """ // mustNot compile
      /*
      Compile error:
      covariant type A occurs in contravariant position in type A of value a
      def encode(a: A): String
      */
    }
  }
}