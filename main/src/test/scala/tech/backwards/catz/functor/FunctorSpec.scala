package tech.backwards.catz.functor

import cats.effect.IO
import cats.effect.unsafe.implicits.global
import cats.{Functor, Id}
import monocle.Lens
import monocle.macros.GenLens
import monocle.macros.syntax.lens._
import org.scalatest.matchers.must.Matchers
import org.scalatest.wordspec.AnyWordSpec

/**
  * A Functor is a type class with a "map" method.
  * So, in simple terms any type constructor (type wrapping another type) that has a "map" method can be thought of as a Functor.
  */
class FunctorSpec extends AnyWordSpec with Matchers {
  case class LineItem(price: Double)

  val priceLineItemLens: Lens[LineItem, Double] =
    GenLens[LineItem](_.price)

  val lineItems: List[LineItem] =
    List(LineItem(10.0), LineItem(20.0))

  "Functor basics" should {
    "list" in {
      import cats.instances.list._

      Functor[List].map(List(1))(_ + 1) mustBe List(2)
    }

    "option" in {
      import cats.instances.option._

      Functor[Option].map(Option("Hello!"))(_.length) mustBe Option(6)
    }
  }

  "Functor" should {
    "step 1 - method overloading" in {
      object Vat {
        val rate = 1.2

        def withVat(orders: List[LineItem]): List[LineItem] =
          orders.map(priceLineItemLens.modify(_ * rate))

        def withVat(maybeOrder: Option[LineItem]): Option[LineItem] =
          maybeOrder.map(priceLineItemLens.modify(_ * rate))

        def withVat(eventualOrder: IO[LineItem]): IO[LineItem] =
          eventualOrder.map(priceLineItemLens.modify(_ * rate))
      }

      import Vat._

      withVat(lineItems) mustBe List(LineItem(10.0 * rate), LineItem(20.0 * rate))
      withVat(Option(LineItem(10.0))) mustBe Option(LineItem(10.0 * rate))
      withVat(IO(LineItem(10.0))).unsafeRunSync() mustBe LineItem(10.0 * rate)

      // But what if we wanted just one "withVat" function that can operate on anything that has "map"?
      // The following does not compile, but it is an illustration of trying to use some top level trait - Functor needs to take a type constructor:
      "def withVat(order: Functor[LineItem]) = order.map(priceLineItemLens.modify(_ * rate))" mustNot compile
    }

    "step 2 - single functions that need a Functor instance" in {
      object Vat {
        val rate = 1.2

        // F is a type constructor such as Option, List, IO etc.
        def withVat[F[_]: Functor](order: F[LineItem]): F[LineItem] =
          Functor[F].map(order)(priceLineItemLens.modify(_ * rate))
      }

      import Vat._

      // The following will not compile because we don't have a Seq/List Functor available:
      // "withVat(lineItems).foreach(println)" mustNot compile // No implicits found for parameter functor$F:Functor[List]
      withVat(lineItems).foreach(println) // TODO - Made some change and now this is compiling when I don't want it to

      import cats.instances.list._
      withVat(lineItems) mustBe Seq(LineItem(10.0 * rate), LineItem(20.0 * rate))

      import cats.instances.option._
      "withVat(Some(LineItem(10.0)))" mustNot compile // // No implicits found for parameter functor$F:Functor[Some]
      // Mmmm. Interesting, even though we brought in option functor instances.
      // We've run into an issue of variance.
      // Cats includes an implementation for Option but we're passing Some.
      // We might expect that Functor[Some] would be treated as Functor[Option] (known as Covariance)
      // but in fact Cats is generally invariant of types i.e. it wants an Option and only an Option, a Some or None won't do.

      // This will work:
      withVat(Option(LineItem(10.0))) mustBe Option(LineItem(10.0 * rate))

      // and this:
      withVat(Some(LineItem(10.0)): Option[LineItem]) mustBe Option(LineItem(10.0 * rate))

      {
        // and we could bring in our own Functor implementation (just keeping out of scope of the above that expects a compilation error):
        implicit val someFunctor: Functor[Some] = new Functor[Some] {
          def map[A, B](fa: Some[A])(f: A => B): Some[B] =
            Some(f(fa.get))
        }

        withVat(Some(LineItem(10.0))) mustBe Option(LineItem(10.0 * rate))
      }

      // While the hack is annoying, we can use cats.syntax.option._
      import cats.syntax.option._
      withVat(LineItem(10.0).some) mustBe Option(LineItem(10.0 * rate))
      // where .some is equivalent to Some(LineItem(10.0)) but it tells the compiler to treat the Some as an Option

      // The above is very useful for testing. Our "withVat" accepts any Functor.
      // If we had hardcoded Future, our test would need and execution context and some "await" or "async spec".
      // Instead we could use Option or better Id instead of Future - or Eval when Id falls short.
      withVat[Id](LineItem(10)) mustBe LineItem(10 * rate)
    }

    "step 3 - syntax extension" in {
      // To avoid:
      // Functor[F].map(order)
      // extension syntax allows:
      // order.map

      import cats.syntax.functor._

      object Vat {
        val rate = 1.2

        // F is a type constructor such as Option, List, IO etc.
        def withVat[F[_]: Functor](order: F[LineItem]): F[LineItem] =
          order.map(priceLineItemLens.modify(_ * rate))
          // instead of:
          // Functor[F].map(order)(priceLineItemLens.modify(_ * rate))
      }
    }

    "step 4 - more generic" in {
      import cats.syntax.functor._

      object Vat {
        val rate = 1.2

        def withFunctor[F[_]: Functor, A, B](order: F[A])(op: A => B): F[B] =
          order.map(op)
      }

      import Vat._
      import cats.instances.list._

      withFunctor(lineItems)(priceLineItemLens.modify(_ * rate)) mustBe List(LineItem(10.0 * rate), LineItem(20.0 * rate))
    }

    "step 5 - compose" in {
      final case class User()

      final case class Order(price: Double)

      val rate = 1.2

      def fetchOrder(user: User): IO[Option[Order]] =
        IO(Option(Order(10.0)))

      def applyVat(order: Order) =
        order.lens(_.price).modify(_ * rate)

      val order: IO[Option[Order]] =
        fetchOrder(User())

      // Ugly nested map
      order.map(_.map(applyVat)).unsafeRunSync() mustBe Option(Order(10.0 * rate))

      // But of course Cats ncludes Functor implementations for both IO and Option which we can compose.
      import cats.Functor
      import cats.instances.option._

      val result: IO[Option[Order]] =
        Functor[IO].compose[Option].map(order)(applyVat)

      result.unsafeRunSync() mustBe Option(Order(10.0 * rate))

      // We can cut down the boilerplate by writing our own implicit class which adds a nestedMap method to all nested Functors:
      implicit class RichFunctor[F[_]: Functor, G[_]: Functor, A](underlying: F[G[A]]) {
        def nestedMap[B](op: A => B): F[G[B]] =
          Functor[F].compose[G].map(underlying)(op)
      }

      order.nestedMap(applyVat).unsafeRunSync() mustBe Option(Order(10.0 * rate))
      // Will work with any Functor combinations
    }
  }
}