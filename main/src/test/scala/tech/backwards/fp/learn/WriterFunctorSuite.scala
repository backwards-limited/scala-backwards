package tech.backwards.fp.learn

import cats.implicits.toShow
import munit.ScalaCheckSuite
import tech.backwards.fp.learn.Writer.tell
import tech.backwards.io.Console.syntax._
import org.scalacheck.Prop._
import org.scalacheck.Test

class WriterFunctorSuite extends ScalaCheckSuite {
  override def scalaCheckTestParameters: Test.Parameters =
    super.scalaCheckTestParameters.withMinSuccessfulTests(100).withMaxDiscardRatio(10)

  property("Writer Functor fmap") {
    assertEquals(
      Functor[Writer[String, *]].fmap(Writer(() => "foo" -> 10))(_ + 1).run(),
      "foo" -> 11
    )

    assertEquals(
      Functor[Writer[List[String], *]].fmap(Writer(() => List("foo") -> 10))(_ + 1).run(),
      List("foo") -> 11
    )
  }

  property("Writer Functor fmap syntax") {
    import tech.backwards.fp.learn.Functor.syntax._

    assertEquals(
      Writer(() => "foo" -> 10).fmap(_ + 1).run(),
      "foo" -> 11
    )
  }

  property("Writer Functor fmap of arbitrary syntax") {
    import tech.backwards.fp.learn.Functor.syntax._

    forAll((x: Int) =>
      assertEquals(
        Writer(() => "foo" -> x).fmap(_ + 1).run(),
        "foo" -> (x + 1)
      )
    )
  }

  property("Writer Functor fmap of function syntax") {
    import tech.backwards.fp.learn.Functor.syntax.function._

    assertEquals(
      { x: Int => x + 1 }.fmap(Writer(() => "foo" -> 10)).run(),
      "foo" -> 11
    )
  }

  property("Writer Functor fmap - obey identity") {
    import tech.backwards.fp.learn.Functor.syntax._

    assertEquals(
      Writer(() => "foo" -> 10).fmap(identity).run(),
      "foo" -> 10
    )
  }

  property("Writer tell") {
    assertEquals(
      tell("foo").run(),
      "foo" -> ()
    )

    assertEquals(
      tell(List("foo")).run(),
      List("foo") -> ()
    )
  }

  property("Writer Functor fmap syntax - obey composition") {
    import tech.backwards.fp.learn.Functor.syntax._

    val f: Int => Int =
      _ + 2

    val g: Int => Int =
      _ * 3

    val writer: Writer[List[String], Int] =
      Writer(() => List("foo") -> 10)

    assertEquals(
      (writer `<$>` f `<$>` g).run().debug(_.yellow),
      writer.fmap(f andThen g).run()
    )
  }
}