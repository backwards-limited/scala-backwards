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
  }

  property("Id Foldable syntax") {
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
  }

  property("List Foldable syntax") {
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

  property("Tuple Foldable") {
    assertEquals(
      Foldable[Lambda[X => (X, X)]].foldr("a" -> "b")("seed")((a, b) => s"$b-$a"),
      "seed-b-a"
    )

    assertEquals(
      Foldable[Lambda[X => (X, X, X)]].foldr(("a", "b", "c"))("seed")((a, b) => s"$b-$a"),
      "seed-c-b-a"
    )
  }

  property("Tuple Foldable syntax") {
    import tech.backwards.fp.learn.Foldable.syntax._

    assertEquals(
      ("a" -> "b").foldr("seed")((a, b) => s"$b-$a"),
      "seed-b-a"
    )

    assertEquals(
      ("a", "b", "c").foldr("seed")((a, b) => s"$b-$a"),
      "seed-c-b-a"
    )
  }

  property("Maybe Foldable") {
    assertEquals(
      Foldable[Maybe].foldr(Just("a"))("seed")((a, b) => s"$b-$a"),
      "seed-a"
    )

    assertEquals(
      Foldable[Maybe].foldr(Nothing[String])("seed")((a, b) => s"$b-$a"),
      "seed"
    )
  }

  property("Maybe Foldable syntax") {
    import tech.backwards.fp.learn.Foldable.syntax._

    assertEquals(
      Just("a").foldr("seed")((a, b) => s"$b-$a"),
      "seed-a"
    )

    assertEquals(
      Nothing[String].foldr("seed")((a, b) => s"$b-$a"),
      "seed"
    )
  }

  property("Disjunction Foldable") {
    assertEquals(
      Foldable[Disjunction[String, *]].foldr(Right("a"))("seed")((a, b) => s"$b-$a"),
      "seed-a"
    )

    assertEquals(
      Foldable[Disjunction[String, *]].foldr(Left[String, String]("whoops"))("seed")((a, b) => s"$b-$a"),
      "seed"
    )
  }

  property("Disjunction Foldable syntax") {
    import tech.backwards.fp.learn.Foldable.syntax._

    assertEquals(
      Right("a").foldr("seed")((a, b) => s"$b-$a"),
      "seed-a"
    )

    assertEquals(
      Nothing[String].foldr("seed")((a, b) => s"$b-$a"),
      "seed"
    )
  }
}