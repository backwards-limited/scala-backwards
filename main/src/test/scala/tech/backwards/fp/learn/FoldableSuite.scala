package tech.backwards.fp.learn

import munit.ScalaCheckSuite
import org.scalacheck.Test

class FoldableSuite extends ScalaCheckSuite {
  override protected def scalaCheckTestParameters: Test.Parameters =
    super.scalaCheckTestParameters.withMinSuccessfulTests(100).withMaxDiscardRatio(10)

  property("Id Foldable") {
    assertEquals(
      Foldable[Id].foldr(Id("a"))("b")((a, b) => s"$b-$a"),
      "b-a"
    )

    import tech.backwards.fp.learn.Foldable.syntax._

    assertEquals(
      Id("a").foldr("b")((a, b) => s"$b-$a"),
      "b-a"
    )
  }

  property("List Foldable") {
    assertEquals(
      Foldable[List].foldr(List("a", "b", "c"))("seed")((a, b) => s"$b-$a"),
      "seed-c-b-a"
    )

    assertEquals(
      Foldable[List].foldr(List.empty[String])("seed")((a, b) => s"$b-$a"),
      "seed"
    )

    import tech.backwards.fp.learn.Foldable.syntax._

    assertEquals(
      List("a", "b", "c").foldr("seed")((a, b) => s"$b-$a"),
      "seed-c-b-a"
    )

    assertEquals(
      List.empty[String].foldr("seed")((a, b) => s"$b-$a"),
      "seed"
    )
  }
}