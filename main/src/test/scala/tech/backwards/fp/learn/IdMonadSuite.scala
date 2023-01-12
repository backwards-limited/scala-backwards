package tech.backwards.fp.learn

import munit.ScalaCheckSuite
import org.scalacheck.Prop._
import org.scalacheck.Test

class IdMonadSuite extends ScalaCheckSuite {
  override def scalaCheckTestParameters: Test.Parameters =
    super.scalaCheckTestParameters.withMinSuccessfulTests(100).withMaxDiscardRatio(10)

  property("Id Monad pure")(
    assertEquals(
      Monad[Id].pure(5),
      Id(5)
    )
  )

  property("Id Monad flatMap")(
    assertEquals(
      Monad[Id].flatMap(Id(5))(x => Id(x + 1)),
      Id(6)
    )
  )

  property("Id Monad flatMap syntax") {
    import tech.backwards.fp.learn.Monad.syntax._

    assertEquals(
      Id(5).flatMap(x => Id(x + 1)),
      Id(6)
    )

    assertEquals(
      Id(5) >>= (x => Id(x + 1)),
      Id(6)
    )
  }

  property("Id Monad flatMap of arbitrary syntax") {
    import tech.backwards.fp.learn.Monad.syntax._

    forAll((x: Int) =>
      assertEquals(
        Id(x) >>= (x => Id(x + 1)),
        Id(x + 1)
      )
    )
  }

  property("Id Monad flatMap of function syntax") {
    import tech.backwards.fp.learn.Monad.syntax.function._

    assertEquals(
      ((x: Int) => Id(x + 1)) flatMap Id(5),
      Id(6)
    )

    assertEquals(
      ((x: Int) => Id(x + 1)) >>= Id(5),
      Id(6)
    )
  }
}