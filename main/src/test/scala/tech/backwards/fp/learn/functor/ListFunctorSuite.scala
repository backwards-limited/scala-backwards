package tech.backwards.fp.learn.functor

import munit.ScalaCheckSuite
import org.scalacheck.Prop._
import org.scalacheck.Test

class ListFunctorSuite extends ScalaCheckSuite {
  override def scalaCheckTestParameters: Test.Parameters =
    super.scalaCheckTestParameters.withMinSuccessfulTests(100).withMaxDiscardRatio(10)

  property("List Functor fmap")(
    assertEquals(
      Functor[List].fmap(List(1, 2, 3))(_ + 1),
      List(2, 3, 4)
    )
  )

  property("List Functor fmap syntax") {
    import tech.backwards.fp.learn.functor.Functor.syntax._

    assertEquals(
      List(1, 2, 3).fmap(_ + 1),
      List(2, 3, 4)
    )

    assertEquals(
      List(1, 2, 3) `<$>` (_ + 1),
      List(2, 3, 4)
    )

    assertEquals(
      List.empty[Int] `<$>` (_ + 1),
      Nil
    )
  }

  // TODO - Would like the following (and in all functor tests) but can't work it out.
  // Functor[Int => Int].fmap

  property("List Functor fmap of function syntax") {
    import tech.backwards.fp.learn.functor.Functor.syntax.function._

    assertEquals(
      { x: Int => x + 1 } fmap List(1, 2, 3),
      List(2, 3, 4)
    )

    assertEquals(
      { x: Int => x + 1 } `<$>` List(1, 2, 3),
      List(2, 3, 4)
    )
  }

  property("List Functor fmap of arbitrary syntax") {
    import tech.backwards.fp.learn.functor.Functor.syntax._

    forAll((xs: List[Int]) =>
      assertEquals(
        xs.fmap(_ + 1),
        xs.map(_ + 1)
      )
    )
  }

  property("List Functor fmap - obey identity") {
    import tech.backwards.fp.learn.functor.Functor.syntax._

    assertEquals(
      List(1, 2, 3) fmap identity,
      List(1, 2, 3)
    )
  }

  property("List Functor fmap syntax - obey composition") {
    import tech.backwards.fp.learn.functor.Functor.syntax._

    val f: Int => Int =
      _ + 2

    val g: Int => Int =
      _ * 3

    assertEquals(
      List(1, 2, 3) `<$>` f fmap g,
      List(1, 2, 3).fmap(f andThen g)
    )
  }
}