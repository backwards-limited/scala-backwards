package tech.backwards.fp.learn.typeclass

import munit.ScalaCheckSuite
import org.scalacheck.Prop._
import org.scalacheck.Test

class TotalOrderSuite extends ScalaCheckSuite {
  override def scalaCheckTestParameters: Test.Parameters =
    super.scalaCheckTestParameters.withMinSuccessfulTests(100).withMaxDiscardRatio(10)

  property("Total order Ints")(
    forAll((x: Int, y: Int) => x < y ==>
      assert(TotalOrder.less(x, y))
    )
  )

  property("Total order Ints with syntax") {
    import tech.backwards.fp.learn.typeclass.TotalOrder.syntax._

    forAll((x: Int, y: Int) => x < y ==>
      assert(x less y)
    )
  }

  property("Total order Strings")(
    assert(TotalOrder.less("4", "5"))
  )

  property("Total order Strings with syntax") {
    import tech.backwards.fp.learn.typeclass.TotalOrder.syntax._

    assert("4" less "5")
  }

  property("Total order List of Int")(
    assert(TotalOrder.less(List(4, 4, 99), List(4, 5, 91)))
  )

  property("Total order List of Int with syntax") {
    import tech.backwards.fp.learn.typeclass.TotalOrder.syntax._

    assert(List(4, 4, 99) less List(4, 5, 91))
  }
}