package tech.backwards.fp.learn

import munit.ScalaCheckSuite
import tech.backwards.io.Console.syntax.{ConsoleSyntax, ConsoleSyntax2}
import org.scalacheck.Prop._
import org.scalacheck.Test

class StateFunctorSuite extends ScalaCheckSuite {
  override def scalaCheckTestParameters: Test.Parameters =
    super.scalaCheckTestParameters.withMinSuccessfulTests(100).withMaxDiscardRatio(10)

  property("State put")(
    assertEquals(
      State.put(20).run(10),
      20 -> ()
    )
  )

  property("State exec")(
    assertEquals(
      State.put(20).exec(10),
      20
    )
  )

  property("State get")(
    assertEquals(
      State.get.run(10),
      10 -> 10
    )
  )

  property("State eval")(
    assertEquals(
      State.get.eval(10),
      10
    )
  )

  property("State modify")(
    assertEquals(
      State.modify[Int](_ + 1).run(10),
      11 -> ()
    )
  )

  property("State Functor fmap")(
    assertEquals(
      Functor[State[String, *]].fmap(State[String, Int](s => s"$s statified" -> 10))(_ + 1).run("state"),
      "state statified" -> 11
    )
  )

  property("State Functor fmap syntax") {
    import tech.backwards.fp.learn.Functor.syntax._

    assertEquals(
      State((s: String) => s"$s statified" -> 10).fmap(_ + 1).run("state"),
      "state statified" -> 11
    )

    assertEquals(
      State((s: String) => s"$s statified" -> 10) `<$>` (_ + 1) run "state",
      "state statified" -> 11
    )
  }

  property("State Functor fmap of arbitrary syntax") {
    import tech.backwards.fp.learn.Functor.syntax._

    forAll((x: Int) =>
      assertEquals(
        State((s: String) => s"$s statified" -> x) `<$>` (_ + 1) run "state",
        "state statified" -> (x + 1)
      )
    )
  }

  property("State Functor fmap of function syntax") {
    import tech.backwards.fp.learn.Functor.syntax.function._

    assertEquals(
      ((x: Int) => x + 1).fmap(State((s: String) => s"$s statified" -> 10)).run("state"),
      "state statified" -> 11
    )

    assertEquals(
      ((x: Int) => x + 1) `<$>` State((s: String) => s"$s statified" -> 10) run "state",
      "state statified" -> 11
    )
  }

  property("State Functor fmap syntax - obey identity") {
    import tech.backwards.fp.learn.Functor.syntax._

    assertEquals(
      State((s: String) => s"$s statified" -> 10) `<$>` identity run "state",
      "state statified" -> 10
    )
  }

  property("State Functor fmap syntax - obey composition") {
    import tech.backwards.fp.learn.Functor.syntax._

    val f: Int => Int =
      _ + 2

    val g: Int => Int =
      _ * 3

    val state: State[String, Int] =
      State((s: String) => s"$s statified" -> 10)

    assertEquals(
      (state `<$>` f `<$>` g run "state").debug(_.yellow),
      state `<$>` (f andThen g) run "state"
    )
  }
}