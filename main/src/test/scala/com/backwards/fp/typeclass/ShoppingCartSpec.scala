package com.backwards.fp.typeclass

import cats.kernel.Semigroup
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.must.Matchers

/**
 * [[https://medium.com/riskified-technology/kittens-high-on-catnip-squeaky-clean-type-class-derivation-in-scala-395eba3c0f4e Kittens High on Catnip - Squeaky Clean Type Class Derivation in Scala]]
 */
class ShoppingCartSpec extends AnyFreeSpec with Matchers {
  "Semigroup" - {
    // We want something like:
    // ShoppingCart(List(CatalogItem("Shirt")), Usd(20)) combine ShoppingCart(List(CatalogItem("Shoes")), Usd(50)) mustBe ShoppingCart(List(CatalogItem("Shirt"), CatalogItem("Shoes")), Usd(70))

    // We ask ourselves whether a general-purpose type class can be used instead.
    // This is exactly what Cats' Semigroup[A] type class represents: it defines a single method named "combine" that enables, well, combining, instances of the type parameter A.

    // We can define a Semigroup[ShoppingCart] instance.
    // Notice that this "combine" operation is recursive in a sense:
    // — combining shopping carts requires combining items lists and Usd prices; which in turn requires combining (i.e. summing) Double values.

    // Fortunately, Cats provides standard Semigroup instances for List (with concatenation) and Double (with addition) out-of-the-box, using a simple import,
    // so you’ll only need to provide the following instances: (note the "|+|" operator syntax, which is a shortcut for Semigroup's "combine"):
    import cats.implicits._
    import cats.Semigroup

    implicit val usdSemigroup: Semigroup[Usd] =
      (x: Usd, y: Usd) => Usd(x.value |+| y.value)

    implicit val shoppingCartSemigroup: Semigroup[ShoppingCart] =
      (x: ShoppingCart, y: ShoppingCart) => ShoppingCart(x.items |+| y.items, x.totalPrice |+| y.totalPrice)

    // Now we can have:
    ShoppingCart(List(CatalogItem("Shirt")), Usd(20)) |+| ShoppingCart(List(CatalogItem("Shoes")), Usd(50)) mustBe ShoppingCart(List(CatalogItem("Shirt"), CatalogItem("Shoes")), Usd(70))

    // This quickly becomes tedious and inefficient — these Semigroup instances are very trivial, and there are just two.
    // Imagine a real-world application where there could be dozens or more!
  }

  "Using Kittens" - {
    // Kittens will automatically create type class instances for your ADTs — all you have to do is add an import statement.

    // We can replace all the Semigroup instances (of which there can be dozens, as you recall) with a single import that will tell kittens to create them automatically, when needed.
    // This reduces the code to simply:
    import cats.implicits._
    import cats.derived.auto.semigroup._

    ShoppingCart(List(CatalogItem("Shirt")), Usd(20)) |+| ShoppingCart(List(CatalogItem("Shoes")), Usd(50)) mustBe ShoppingCart(List(CatalogItem("Shirt"), CatalogItem("Shoes")), Usd(70))
  }

  "Using more performant Kittens" - {
    // There are several "modes" of automatic derivation, where the full-auto mode will re-derive for every use site (even if an identical instance was already derived in another place).
    // This can have a very significant impact on the project’s compilation time.
    // The recommended best practice is to use semi-auto derivation instead, which would look like this:
    import cats.derived
    import cats.implicits._

    implicit val shoppingCartSemigroup: Semigroup[ShoppingCart] = {
      import derived.auto.semigroup._

      derived.semiauto.semigroup
    }
    // This fixes the issue of repeated re-derivation, by manually caching the type class instance in the implicit val.

    ShoppingCart(List(CatalogItem("Shirt")), Usd(20)) |+| ShoppingCart(List(CatalogItem("Shoes")), Usd(50)) mustBe ShoppingCart(List(CatalogItem("Shirt"), CatalogItem("Shoes")), Usd(70))
  }

  "Good but we seem to be back to some boilerplate - this can be overcome via Catnip" - {
    // Catnip provides class annotations as an alternative to writing kittens semi-auto derivation code by hand.
    // The annotations are very simple and clean (@Semi for "semi-auto"):
    import cats.implicits._
    import io.scalaland.catnip.Semi

    @Semi(Semigroup) final case class CatalogItem(value: String) // extends AnyVal REMOVED BECAUSE IT IS LOCAL

    @Semi(Semigroup) final case class Usd(value: Double) // extends AnyVal REMOVED BECAUSE IT IS LOCAL

    @Semi(Semigroup) final case class ShoppingCart(items: List[CatalogItem], totalPrice: Usd)

    ShoppingCart(List(CatalogItem("Shirt")), Usd(20)) |+| ShoppingCart(List(CatalogItem("Shoes")), Usd(50)) mustBe ShoppingCart(List(CatalogItem("Shirt"), CatalogItem("Shoes")), Usd(70))
  }
}

final case class CatalogItem(value: String) extends AnyVal

final case class Usd(value: Double) extends AnyVal

final case class ShoppingCart(items: List[CatalogItem], totalPrice: Usd)