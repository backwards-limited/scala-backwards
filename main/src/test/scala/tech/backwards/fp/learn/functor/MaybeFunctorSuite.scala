package tech.backwards.fp.learn.functor

import munit.ScalaCheckSuite
import org.scalacheck.Prop._
import org.scalacheck.Test

class MaybeFunctorSuite extends ScalaCheckSuite {
  override def scalaCheckTestParameters: Test.Parameters =
    super.scalaCheckTestParameters.withMinSuccessfulTests(100).withMaxDiscardRatio(10)

  property("Just Functor fmap")(
    assertEquals(
      Functor[Just].fmap(Just(1))(_ + 1),
      Just(2)
    )
  )

  property("Just Functor fmap syntax") {
    import tech.backwards.fp.learn.functor.Functor.syntax._

    assertEquals(
      Just(1).fmap(_ + 1),
      Just(2)
    )

    assertEquals(
      Just(1) `<$>` (_ + 1),
      Just(2)
    )
  }

  property("Just Functor fmap of function syntax") {
    import tech.backwards.fp.learn.functor.Functor.syntax.function._

    assertEquals(
      { x: Int => x + 1 } fmap Just(1),
      Just(2)
    )

    assertEquals(
      { x: Int => x + 1 } `<$>` Just(1),
      Just(2)
    )
  }

  property("Just Functor fmap of arbitrary syntax") {
    import tech.backwards.fp.learn.functor.Functor.syntax._

    forAll((x: Int) =>
      assertEquals(
        Just(x).fmap(_ + 1),
        Just(x + 1)
      )
    )
  }

  property("Nothing Functor fmap")(
    assertEquals(
      Functor[Nothing].fmap(Nothing[Int])(_ + 1),
      Nothing[Int]
    )
  )

  property("Nothing Functor fmap syntax") {
    import tech.backwards.fp.learn.functor.Functor.syntax._

    assertEquals(
      Nothing[Int].fmap(_ + 1),
      Nothing[Int]
    )

    assertEquals(
      Nothing[Int] `<$>` (_ + 1),
      Nothing[Int]
    )
  }

  property("Nothing Functor fmap of function syntax") {
    import tech.backwards.fp.learn.functor.Functor.syntax.function._

    assertEquals(
      { x: Int => x + 1 } fmap Nothing[Int],
      Nothing[Int]
    )

    assertEquals(
      { x: Int => x + 1 } `<$>` Nothing[Int],
      Nothing[Int]
    )
  }

  property("Maybe Functor fmap - obey identity") {
    import tech.backwards.fp.learn.functor.Functor.syntax._

    assertEquals(
      Just(1) fmap identity,
      Just(1)
    )
    
    assertEquals(
      Nothing[Int] `<$>` identity,
      Nothing[Int]
    )
  }

  property("Maybe Functor fmap syntax - obey composition") {
    import tech.backwards.fp.learn.functor.Functor.syntax._

    val f: Int => Int =
      _ + 2

    val g: Int => Int =
      _ * 3

    assertEquals(
      Just(2) `<$>` f fmap g,
      Just(2).fmap(f andThen g)
    )

    assertEquals(
      Nothing[Int] `<$>` f fmap g,
      Nothing[Int].fmap(f andThen g)
    )
  }
}