package tech.backwards.fp.learn

import munit.ScalaCheckSuite
import tech.backwards.fp.learn.Writer.tell
import tech.backwards.io.Console.syntax.{ConsoleSyntax, ConsoleSyntax2}
import org.scalacheck.Prop._
import org.scalacheck.Test

class WriterMonadSuite extends ScalaCheckSuite {
  override def scalaCheckTestParameters: Test.Parameters =
    super.scalaCheckTestParameters.withMinSuccessfulTests(100).withMaxDiscardRatio(10)

  property("Writer Monad pure") {
    assertEquals(
      Monad[Writer[String, *]].pure(5).run(),
      Monoid[String].mzero -> 5
    )

    assertEquals(
      Monad[Writer[List[String], *]].pure(5).run(),
      Monoid[List[String]].mzero -> 5
    )
  }

  property("Writer Monad flatMap") {
    import tech.backwards.fp.learn.Functor.syntax._

    assertEquals(
      Monad[Writer[String, *]].flatMap(tell[String].as(5))(x => tell[String].as(x + 1)).run(),
      Monoid[String].mzero -> 6
    )
  }

  property("Writer Monad flatMap syntax") {
    import tech.backwards.fp.learn.Monad.syntax._

    assertEquals(
      Writer(() => "foo" -> 10).flatMap(x => Writer(() => "bar" -> (x + 1))).run(),
      "foobar" -> 11
    )

    assertEquals(
      Writer(() => List("foo") -> 10).flatMap(x => Writer(() => List("bar") -> (x + 1))).run(),
      List("foo", "bar") -> 11
    )
  }

  property("Writer Monad pure and flatMap syntax") {
    import tech.backwards.fp.learn.Functor.syntax._
    import tech.backwards.fp.learn.Monad.syntax._

    assertEquals(
      5.pure[Writer[String, *]].flatMap(x => tell[String].as(x + 1)).run(),
      Monoid[String].mzero -> 6
    )
  }

  property("Writer for comprehension syntax") {
    import tech.backwards.fp.learn.Functor.syntax._
    import tech.backwards.fp.learn.Monad.syntax._

    val writer: Writer[List[String], Int] =
      for {
        x <- 1.pure[Writer[List[String], *]]
        _ <- tell(List("one"))
        y <- 2.pure[Writer[List[String], *]]
        _ <- tell(List("two"))
        z <- 3.pure[Writer[List[String], *]]
        _ <- tell(List("three"))
      } yield x + y + z

    assertEquals(
      writer.run(),
      List("one", "two", "three") -> 6
    )
  }

  property("Writer for comprehension syntax") {
    import tech.backwards.fp.learn.Functor.syntax._
    import tech.backwards.fp.learn.Monad.syntax._

    type Writer[A] = tech.backwards.fp.learn.Writer[List[String], A]

    val writer: Writer[Int] =
      for {
        x <- 1.pure[Writer]
        _ <- tell(List("one"))
        y <- 2.pure[Writer]
        _ <- tell(List("two"))
        z <- 3.pure[Writer]
        _ <- tell(List("three"))
      } yield x + y + z

    assertEquals(
      writer.run(),
      List("one", "two", "three") -> 6
    )
  }

  property("Writer Monad flatMap of arbitrary syntax") {
    import tech.backwards.fp.learn.Monad.syntax._

    forAll((x: Int) =>
      assertEquals(
        Writer(() => "foo" -> x).flatMap(x => Writer(() => "bar" -> (x + 1))).run(),
        ("foobar" -> (x + 1)).debug(_.yellow)
      )
    )
  }

  property("Writer Monad flatMap of function syntax") {
    import tech.backwards.fp.learn.Monad.syntax.function._

    assertEquals(
      { x: Int => Writer(() => "bar" -> (x + 1)) }.flatMap(Writer(() => "foo" -> 5)).run(),
      "foobar" -> 6
    )
  }
}