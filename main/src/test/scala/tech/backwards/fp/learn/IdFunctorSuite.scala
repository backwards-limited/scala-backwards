package tech.backwards.fp.learn

import munit.ScalaCheckSuite
import org.scalacheck.Prop._
import org.scalacheck.Test

class IdFunctorSuite extends ScalaCheckSuite {
  override def scalaCheckTestParameters: Test.Parameters =
    super.scalaCheckTestParameters.withMinSuccessfulTests(100).withMaxDiscardRatio(10)

  property("Functor fmap of Id")(
    assertEquals(
      Functor[Id].fmap(Id(1))(_ + 1),
      Id(2)
    )
  )

  property("Functor fmap of Id syntax") {
    import tech.backwards.fp.learn.Functor.syntax._

    assertEquals(
      Id(1).fmap(_ + 1),
      Id(2)
    )

    assertEquals(
      Id(1) `<$>` (_ + 1),
      Id(2)
    )
  }

  property("Functor fmap of function for Id syntax") {
    import tech.backwards.fp.learn.Functor.syntax.function._

    assertEquals(
      ((x: Int) => x + 1) fmap Id(1),
      Id(2)
    )

    assertEquals(
      ((x: Int) => x + 1) `<$>` Id(1),
      Id(2)
    )
  }

  property("Functor fmap of arbitrary Id syntax") {
    import tech.backwards.fp.learn.Functor.syntax._

    forAll((x: Int) =>
      assertEquals(
        Id(x).fmap(_ + 1),
        Id(x + 1)
      )
    )
  }

  property("Functor fmap of Id syntax - obey identity") {
    import tech.backwards.fp.learn.Functor.syntax._

    assertEquals(
      Id(1) fmap identity,
      Id(1)
    )
  }

  property("Functor fmap of Id syntax - obey composition") {
    import tech.backwards.fp.learn.Functor.syntax._

    val f: Int => Int =
      _ + 1

    val g: Int => Int =
      _ * 2

    assertEquals(
      Id(2) fmap f fmap g,
      Id(2).fmap(f andThen g)
    )
  }
}