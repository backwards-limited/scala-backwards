package tech.backwards.fp.learn.monoid

import munit.ScalaCheckSuite
import org.scalacheck.Prop._
import org.scalacheck.{Arbitrary, Gen, Test}

class MonoidSuite extends ScalaCheckSuite {
  override def scalaCheckTestParameters: Test.Parameters =
    super.scalaCheckTestParameters.withMinSuccessfulTests(100).withMaxDiscardRatio(10)

  property("Sum mzero")(
    assertEquals(Monoid[Sum].mzero, Sum(0))
  )

  property("Sum mappend")(
    assertEquals(
      Monoid[Sum].mappend(Sum(1), Sum(2)),
      Sum(3)
    )
  )

  property("Sum mappend syntax") {
    import tech.backwards.fp.learn.monoid.Monoid.syntax._

    assertEquals(
      Sum(1) |+| Sum(2),
      Sum(3)
    )
  }

  property("Sum arbitrary mappend syntax") {
    import tech.backwards.fp.learn.monoid.Monoid.syntax._

    forAll((x: Int, y: Int) =>
      assertEquals(
        Sum(x) |+| Sum(y),
        Sum(x + y)
      )
    )
  }

  property("Sum mappend syntax - obey identity") {
    import tech.backwards.fp.learn.monoid.Monoid.syntax._

    assertEquals(
      Sum(1) |+| Monoid[Sum].mzero,
      Sum(1)
    )

    assertEquals(
      Monoid[Sum].mzero |+| Sum(1),
      Sum(1)
    )
  }

  property("Sum mappend syntax - obey associativity") {
    import tech.backwards.fp.learn.monoid.Monoid.syntax._

    assertEquals(
      Sum(1) |+| Sum(2) |+| Sum(3),
      Sum(6)
    )

    assertEquals(
      Sum(1) |+| (Sum(2) |+| Sum(3)),
      Sum(6)
    )
  }

  property("Product mzero")(
    assertEquals(Monoid[Product].mzero, Product(1))
  )

  property("Product mappend")(
    assertEquals(
      Monoid[Product].mappend(Product(1), Product(2)),
      Product(2)
    )
  )

  property("Product mappend syntax") {
    import tech.backwards.fp.learn.monoid.Monoid.syntax._

    assertEquals(
      Product(1) |+| Product(2),
      Product(2)
    )
  }

  property("Product arbitrary mappend syntax") {
    import tech.backwards.fp.learn.monoid.Monoid.syntax._

    forAll((x: Int, y: Int) =>
      assertEquals(
        Product(x) |+| Product(y),
        Product(x * y)
      )
    )
  }

  property("Product mappend syntax - obey identity") {
    import tech.backwards.fp.learn.monoid.Monoid.syntax._

    assertEquals(
      Product(5) |+| Monoid[Product].mzero,
      Product(5)
    )

    assertEquals(
      Monoid[Product].mzero |+| Product(5),
      Product(5)
    )
  }

  property("Product mappend syntax - obey associativity") {
    import tech.backwards.fp.learn.monoid.Monoid.syntax._

    assertEquals(
      Product(5) |+| Product(2) |+| Product(3),
      Product(30)
    )

    assertEquals(
      Product(5) |+| (Product(2) |+| Product(3)),
      Product(30)
    )
  }

  property("List mzero")(
    assertEquals(Monoid[List[Int]].mzero, Nil)
  )

  property("List mappend")(
    assertEquals(
      Monoid[List[Int]].mappend(List(1, 2, 3), List(4, 5, 6)),
      List(1, 2, 3, 4, 5, 6)
    )
  )

  property("List mappend syntax") {
    import tech.backwards.fp.learn.monoid.Monoid.syntax._

    assertEquals(
      List(1, 2, 3) |+| List(4, 5, 6),
      List(1, 2, 3, 4, 5, 6)
    )
  }

  property("List arbitrary mappend syntax") {
    import tech.backwards.fp.learn.monoid.Monoid.syntax._

    forAll((xs: List[Int], ys: List[Int]) =>
      assertEquals(
        xs |+| ys,
        xs ++ ys
      )
    )
  }

  property("List mappend syntax - obey identity") {
    import tech.backwards.fp.learn.monoid.Monoid.syntax._

    assertEquals(
      List(1, 2, 3) |+| Monoid[List[Int]].mzero,
      List(1, 2, 3)
    )

    assertEquals(
      Monoid[List[Int]].mzero |+| List(1, 2, 3),
      List(1, 2, 3)
    )
  }

  property("List mappend syntax - obey associativity") {
    import tech.backwards.fp.learn.monoid.Monoid.syntax._

    assertEquals(
      (List(1, 2, 3) |+| List(4, 5, 6)) |+| List(7, 8, 9),
      List(1, 2, 3, 4, 5, 6, 7, 8, 9)
    )

    assertEquals(
      List(1, 2, 3) |+| (List(4, 5, 6) |+| List(7, 8, 9)),
      List(1, 2, 3, 4, 5, 6, 7, 8, 9)
    )
  }

  property("Pair mzero")(
    assertEquals(
      Monoid[Pair[Sum, Product]].mzero,
      Pair(Monoid[Sum].mzero, Monoid[Product].mzero)
    )
  )

  property("Pair mappend")(
    assertEquals(
      Monoid[Pair[Sum, Product]].mappend(Pair(Sum(3), Product(3)), Pair(Sum(5), Product(6))),
      Pair(Sum(3 + 5), Product(3 * 6))
    )
  )

  property("Pair mappend syntax") {
    import tech.backwards.fp.learn.monoid.Monoid.syntax._

    assertEquals(
      Pair(Sum(3), Product(3)) |+| Pair(Sum(5), Product(6)),
      Pair(Sum(3 + 5), Product(3 * 6))
    )
  }

  property("Pair arbitrary mappend syntax") {
    import tech.backwards.fp.learn.monoid.Monoid.syntax._

    implicit val arbitraryPair: Arbitrary[Pair[Sum, Product]] =
      Arbitrary(
        Gen.posNum[Int].map(Sum).flatMap(sum =>
          Gen.posNum[Int].map(Product).map(product =>
            Pair(sum, product)
          )
        )
      )

    forAll((pair1: Pair[Sum, Product], pair2: Pair[Sum, Product]) =>
      assertEquals(
        pair1 |+| pair2,
        Pair(Sum(pair1.a.value + pair2.a.value), Product(pair1.b.value * pair2.b.value))
      )
    )
  }

  property("Pair mappend syntax on nested structure") {
    import tech.backwards.fp.learn.monoid.Monoid.syntax._

    assertEquals(
      Pair(Pair(Sum(1),  Product(2)),  Pair(List(3, 33, 333),    Sum(4))) |+|
      Pair(Pair(Sum(21), Product(22)), Pair(List(23, 233, 2333), Sum(24))),

      Pair(Pair(Sum(1 + 21), Product(2 * 22)), Pair(List(3, 33, 333, 23, 233, 2333), Sum(4 + 24)))
    )
  }

  property("Pair mappend syntax - obey identity") {
    import tech.backwards.fp.learn.monoid.Monoid.syntax._

    assertEquals(
      Pair(Sum(10), Product(20)) |+| Monoid[Pair[Sum, Product]].mzero,
      Pair(Sum(10), Product(20))
    )

    assertEquals(
      Pair(List(1, 2, 3), Sum(1)) |+| Monoid[Pair[List[Int], Sum]].mzero,
      Pair(List(1, 2, 3), Sum(1))
    )

    assertEquals(
      Pair(Pair(Sum(1), Sum(2)), Pair(Sum(3), Sum(4))) |+| Monoid[Pair[Pair[Sum, Sum], Pair[Sum, Sum]]].mzero,
      Pair(Pair(Sum(1), Sum(2)), Pair(Sum(3), Sum(4)))
    )
  }

  property("Pair mappend syntax - obey associativity") {
    import tech.backwards.fp.learn.monoid.Monoid.syntax._

    val a: Pair[Pair[Sum, Product], Pair[List[Int], Sum]] =
      Pair(
        Pair(Sum(1), Product(2)),
        Pair(List(3, 33, 333), Sum(4))
      )

    val b: Pair[Pair[Sum, Product], Pair[List[Int], Sum]] =
      Pair(
        Pair(Sum(21), Product(22)),
        Pair(List(23, 233, 2333), Sum(24))
      )

    val c: Pair[Pair[Sum, Product], Pair[List[Int], Sum]] =
      Pair(
        Pair(Sum(31), Product(32)),
        Pair(List(33, 333, 3333), Sum(34))
      )
    
    assertEquals(
      a |+| b |+| c,
      a |+| (b |+| c)
    )
  }
}