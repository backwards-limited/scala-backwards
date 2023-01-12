package tech.backwards.fp.learn

import munit.ScalaCheckSuite
import tech.backwards.io.Console.syntax._
import org.scalacheck.Prop._
import org.scalacheck.Test

class StateMonadSuite extends ScalaCheckSuite {
  override def scalaCheckTestParameters: Test.Parameters =
    super.scalaCheckTestParameters.withMinSuccessfulTests(100).withMaxDiscardRatio(10)

  property("State Monad pure") {
    assertEquals(
      Monad[State[String, *]].pure(5).run("hello"),
      ("hello" -> 5).debug(_.yellow)
    )

    assertEquals(
      Monad[State[List[String], *]].pure(5).run(List("hello")),
      (List("hello") -> 5).debug(_.cyan)
    )
  }

  property("State Monad flatMap") {
    assertEquals(
      Monad[State[String, *]].flatMap(State(_ -> 5))(x => State(_ + " world" -> (x + 1))).run("hello"),
      "hello world" -> 6
    )

    assertEquals(
      Monad[State[List[String], *]].flatMap(State(_ -> 5))(x => State(_ ++ List("world") -> (x + 1))).run(List("hello")),
      List("hello", "world") -> 6
    )
  }

  property("State Monad flatMap syntax") {
    import tech.backwards.fp.learn.Functor.syntax._
    import tech.backwards.fp.learn.Monad.syntax._

    assertEquals(
      State.put("foo").as(10).flatMap(x => State(s => (s + "bar") -> (x + 1))).run("ignore"),
      "foobar" -> 11
    )

    assertEquals(
      (State.put("foo").as(10) >>= (x => State( s => (s + "bar") -> (x + 1)))) run "ignore",
      "foobar" -> 11
    )
  }

  property("State Monad pure and flatMap syntax") {
    import tech.backwards.fp.learn.Monad.syntax._

    assertEquals(
      5.pure[State[String, *]].flatMap(x => State(s => (s + "bar") -> (x + 1))).run("foo"),
      "foobar" -> 6
    )
  }

  property("State for comprehension syntax") {
    import tech.backwards.fp.learn.Functor.syntax._
    import tech.backwards.fp.learn.Monad.syntax._

    val program: State[List[String], Int] =
      for {
        x <- 1.pure[State[List[String], *]]
        sx <- State.get[List[String]]
        y <- 2.pure[State[List[String], *]]
        _ <- State.put[List[String]](sx :+ "foo")
        z <- 3.pure[State[List[String], *]]
        _ <- State.modify[List[String]](_ :+ "bar")
      } yield x + y + z

    assertEquals(
      program run Nil,
      List("foo", "bar") -> 6
    )
  }

  property("State for comprehension syntax") {
    import tech.backwards.fp.learn.Functor.syntax._
    import tech.backwards.fp.learn.Monad.syntax._

    type State[A] = tech.backwards.fp.learn.State[List[String], A]

    val program: State[Int] =
      for {
        x <- 1.pure[State]
        sx <- State.get[List[String]]
        y <- 2.pure[State]
        _ <- State.put[List[String]](sx :+ "foo")
        z <- 3.pure[State]
        _ <- State.modify[List[String]](_ :+ "bar")
      } yield x + y + z

    assertEquals(
      program run List(">>"),
      List(">>", "foo", "bar") -> 6
    )
  }

  property("State Monad flatMap of arbitrary syntax") {
    import tech.backwards.fp.learn.Monad.syntax._

    forAll((x: Int) =>
      assertEquals(
        State((s: String) => s"$s foo" -> x).flatMap(x => State(s => s"$s bar" -> (x + 1))).run(">>"),
        ">> foo bar" -> (x + 1)
      )
    )
  }

  property("State Monad flatMap of function syntax") {
    import tech.backwards.fp.learn.Monad.syntax.function._

    assertEquals(
      ((x: Int) => State((s: String) => s"$s bar" -> (x + 1))).flatMap(State(s => s"$s foo" -> 5)).run(">>"),
      ">> foo bar" -> 6
    )
  }
}