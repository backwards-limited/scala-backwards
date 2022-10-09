package tech.backwards.fp.learn.typeclass

import munit.ScalaCheckSuite
import org.scalacheck.Prop._
import org.scalacheck.Test

class ShowSuite extends ScalaCheckSuite {
  override def scalaCheckTestParameters: Test.Parameters =
    super.scalaCheckTestParameters.withMinSuccessfulTests(100).withMaxDiscardRatio(10)

  property("Show Int")(
    forAll((x: Int) =>
      assertEquals(
        Show.show(x),
        s"$x"
      )
    )
  )

  property("Show Int with syntax") {
    import tech.backwards.fp.learn.typeclass.Show.syntax._

    forAll((x: Int) =>
      assertEquals(
        x.show,
        s"$x"
      )
    )
  }

  property("Show Double")(
    forAll((x: Double) =>
      assertEquals(
        Show.show(x),
        s"$x"
      )
    )
  )

  property("Show Double with syntax") {
    import tech.backwards.fp.learn.typeclass.Show.syntax._

    forAll((x: Double) =>
      assertEquals(
        x.show,
        s"$x"
      )
    )
  }

  property("Show List of Int")(
    forAll((xs: List[Int]) =>
      assertEquals(
        Show.show(xs),
        xs.mkString("[", ", ", "]")
      )
    )
  )

  property("Show List of Int with syntax") {
    import tech.backwards.fp.learn.typeclass.Show.syntax._

    forAll((xs: List[Int]) =>
      assertEquals(
        xs.show,
        xs.mkString("[", ", ", "]")
      )
    )
  }
}