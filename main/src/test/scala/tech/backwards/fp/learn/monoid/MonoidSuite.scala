package tech.backwards.fp.learn.monoid

import munit.ScalaCheckSuite
import org.scalacheck.Prop._
import org.scalacheck.Test

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
}