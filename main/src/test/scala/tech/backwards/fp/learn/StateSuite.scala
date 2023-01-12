package tech.backwards.fp.learn

import munit.ScalaCheckSuite
import org.scalacheck.Test

class StateSuite extends ScalaCheckSuite {
  override def scalaCheckTestParameters: Test.Parameters =
    super.scalaCheckTestParameters.withMinSuccessfulTests(100).withMaxDiscardRatio(10)

  property("State get")(
    assertEquals(
      State.get[Int].run(6),
      6 -> 6
    )
  )

  property("State put") {
    import tech.backwards.fp.learn.Functor.syntax._

    assertEquals(
      State.put(5).run(6),
      5 -> ()
    )

    assertEquals(
      State.put(5).as("foo").run(6),
      5 -> "foo"
    )
  }

  property("State modify") {
    assertEquals(
      State modify ((s: Int) => s + 1) run 5,
      6 -> ()
    )
  }
}