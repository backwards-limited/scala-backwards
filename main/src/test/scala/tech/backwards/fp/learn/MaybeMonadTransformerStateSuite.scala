package tech.backwards.fp.learn

import munit.ScalaCheckSuite
import org.scalacheck.Test

class MaybeMonadTransformerStateSuite extends ScalaCheckSuite {
  override protected def scalaCheckTestParameters: Test.Parameters =
    super.scalaCheckTestParameters.withMinSuccessfulTests(100).withMaxDiscardRatio(10)

  property("MaybeT") {
    val transformer: MaybeT[State[String, *], Int] =
      MaybeT(State(_ -> Just(10)))

    assertEquals(
      transformer.value.run("foo"),
      "foo" -> Just(10)
    )

    assertEquals(
      MaybeT(State((_: String) -> Just(10))).value.run("foo"),
      "foo" -> Just(10)
    )

    assertEquals(
      MaybeT(State((_: String) -> Nothing[Int])).value.run("foo"),
      "foo" -> Nothing[Int]
    )

    assertEquals(
      MaybeT(State((xs: List[String]) => xs ++ List("bar") -> Nothing[Int])).value.run(List("foo")),
      List("foo", "bar") -> Nothing[Int]
    )

    assertEquals(
      MaybeT(State[List[String], Maybe[Int]](_ ++ List("bar") -> Nothing[Int])).value.run(List("foo")),
      List("foo", "bar") -> Nothing[Int]
    )
  }

  property("MaybeT pure") {
    val transformer: MaybeT[State[String, *], Int] =
      MaybeT.pure[State[String, *], Int](10)

    assertEquals(
      transformer.value.run("foo"),
      "foo" -> Just(10)
    )

    assertEquals(
      MaybeT.pure[State[List[String], *], Int](10).value.run(List("foo")),
      List("foo") -> Just(10)
    )
  }

  property("MaybeT lift") {
    val transformer: MaybeT[State[String, *], Int] =
      MaybeT.lift(State(_ -> 10))

    assertEquals(
      transformer.value.run("foo"),
      "foo" -> Just(10)
    )

    assertEquals(
      MaybeT.lift(State((_: List[String]) ++ List("bar") -> 10)).value.run(List("foo")),
      List("foo", "bar") -> Just(10)
    )
  }

  property("MaybeT Functor") {
    val transformer: MaybeT[State[String, *], Int] =
      MaybeT(State(_ -> Just(10)))

    assertEquals(
      Functor[MaybeT[State[String, *], *]].fmap(transformer)(_ + 1).value.run("foo"),
      "foo" -> Just(11)
    )

    assertEquals(
      Functor[MaybeT[State[List[String], *], *]].fmap(MaybeT(State((_: List[String]) ++ List("bar") -> Just(10))))(_ + 1).value.run(List("foo")),
      List("foo", "bar") -> Just(11)
    )

    assertEquals(
      Functor[MaybeT[State[List[String], *], *]].fmap(MaybeT(State(_ -> Nothing[Int])))(_ + 1).value.run(List("foo")),
      List("foo") -> Nothing[Int]
    )
  }

  property("MaybeT Functor syntax") {
    import tech.backwards.fp.learn.Functor.syntax._
    import tech.backwards.fp.learn.Maybe.syntax._

    val transformer: MaybeT[State[String, *], Int] =
      MaybeT(State(_ + "bar" -> 10.just))

    assertEquals(
      transformer.fmap(_ + 1).value.run("foo"),
      "foobar" -> 11.just
    )

    assertEquals(
      MaybeT(State((_: List[String]) ++ List("bar") -> 10.just)).fmap(_ + 1).value.run(List("foo")),
      List("foo", "bar") -> 11.just
    )

    assertEquals(
      MaybeT(State((_: List[String]) -> Nothing[Int])).fmap(_ + 1).value.run(Nil),
      Nil -> Nothing[Int]
    )
  }

  property("MaybeT Monad") {
    val transformer: MaybeT[State[String, *], Int] =
      Monad[MaybeT[State[String, *], *]].pure(10)

    assertEquals(
      Monad[MaybeT[State[String, *], *]].flatMap(transformer)(a => Monad[MaybeT[State[String, *], *]].pure(a + 1)).value.run("foo"),
      "foo" -> Just(11)
    )

    assertEquals(
      Monad[MaybeT[State[List[String], *], *]].flatMap(Monad[MaybeT[State[List[String], *], *]].pure(10))(a => Monad[MaybeT[State[List[String], *], *]].pure(a + 1)).value.run(List("foo")),
      List("foo") -> Just(11)
    )

    assertEquals(
      Monad[MaybeT[State[List[String], *], *]].flatMap(MaybeT(State((_: List[String]) ++ List("bar") -> Nothing[Int])))(a => Monad[MaybeT[State[List[String], *], *]].pure(a + 1)).value.run(List("foo")),
      List("foo", "bar") -> Nothing[Int]
    )

    assertEquals(
      Monad[MaybeT[State[List[String], *], *]].flatMap(Monad[MaybeT[State[List[String], *], *]].pure(10))(_ => MaybeT(State((_: List[String]) -> Nothing[Int]))).value.run(List("foo")),
      List("foo") -> Nothing[Int]
    )

    assertEquals(
      Monad[MaybeT[State[List[String], *], *]].flatMap(MaybeT(State((_: List[String]) -> Just(1))))(a => MaybeT(State((_: List[String]) -> Just(a + 1)))).value.run(List("foo")),
      List("foo") -> Just(2)
    )

    assertEquals(
      Monad[MaybeT[State[List[String], *], *]].flatMap(MaybeT(State((_: List[String]) ++ List("bar") -> Just(1))))(a => MaybeT(State((_: List[String]) ++ List("baz") -> Just(a + 1)))).value.run(List("foo")),
      List("foo", "bar", "baz") -> Just(2)
    )
  }

  property("MaybeT Monad syntax") {
    import tech.backwards.fp.learn.Maybe.syntax._
    import tech.backwards.fp.learn.Monad.syntax._

    val transformer: MaybeT[State[String, *], Int] =
      10.pure[MaybeT[State[String, *], *]]

    assertEquals(
      transformer.flatMap(a => (a + 1).pure[MaybeT[State[String, *], *]]).value.run("foo"),
      "foo" -> 11.just
    )

    assertEquals(
      10.pure[MaybeT[State[List[String], *], *]].flatMap(a => (a + 1).pure[MaybeT[State[List[String], *], *]]).value.run(List("foo")),
      List("foo") -> 11.just
    )

    assertEquals(
      MaybeT(State((_: List[String]) ++ List("bar") -> nothing[Int])).flatMap(a => (a + 1).pure[MaybeT[State[List[String], *], *]]).value.run(List("foo")),
      List("foo", "bar") -> nothing[Int]
    )

    assertEquals(
      10.pure[MaybeT[State[List[String], *], *]].flatMap(_ => MaybeT(State(_ -> nothing[Int]))).value.run(List("foo")),
      List("foo") -> nothing[Int]
    )

    assertEquals(
      MaybeT(State((_: List[String]) -> Just(1))).flatMap(a => MaybeT(State(_ -> Just(a + 1)))).value.run(List("foo")),
      List("foo") -> Just(2)
    )

    assertEquals(
      MaybeT(State((_: List[String]) ++ List("bar") -> Just(1))).flatMap(a => MaybeT(State(_ ++ List("baz") -> Just(a + 1)))).value.run(List("foo")),
      List("foo", "bar", "baz") -> Just(2)
    )
  }

  property("MaybeT Applicative") {
    val transformerFn: MaybeT[State[String, *], Int => Int] =
      Applicative[MaybeT[State[String, *], *]].pure(_ + 1)

    val transformer: MaybeT[State[String, *], Int] =
      Applicative[MaybeT[State[String, *], *]].pure(10)

    assertEquals(
      Applicative[MaybeT[State[String, *], *]].ap(transformerFn)(transformer).value.run("foo"),
      "foo" -> Just(11)
    )

    assertEquals(
      Applicative[MaybeT[State[List[String], *], *]].ap(Applicative[MaybeT[State[List[String], *], *]].pure((x: Int) => x + 1))(Applicative[MaybeT[State[List[String], *], *]].pure(10)).value.run(List("foo")),
      List("foo") -> Just(11)
    )

    assertEquals(
      Applicative[MaybeT[State[List[String], *], *]].ap(MaybeT(State(_ -> Nothing[Int => Int])))(Applicative[MaybeT[State[List[String], *], *]].pure(10)).value.run(List("foo")),
      List("foo") -> Nothing[Int]
    )

    assertEquals(
      Applicative[MaybeT[State[List[String], *], *]].ap(Applicative[MaybeT[State[List[String], *], *]].pure((x: Int) => x + 1))(MaybeT(State(_ -> Nothing[Int]))).value.run(List("foo")),
      List("foo") -> Nothing[Int]
    )

    assertEquals(
      Applicative[MaybeT[State[List[String], *], *]].ap(MaybeT(State((_: List[String]) ++ List("bar") -> Just((x: Int) => x + 1))))(MaybeT(State((_: List[String]) ++ List("baz") -> Nothing[Int]))).value.run(List("foo")),
      List("foo", "bar", "baz") -> Nothing[Int]
    )

    assertEquals(
      Applicative[MaybeT[State[List[String], *], *]].ap(MaybeT(State(_ -> Just((x: Int) => x + 1))))(MaybeT(State(_ -> Just(1)))).value.run(List("foo")),
      List("foo") -> Just(2)
    )

    assertEquals(
      Applicative[MaybeT[State[List[String], *], *]].ap(MaybeT(State((_: List[String]) ++ List("bar") -> Just((x: Int) => x + 1))))(MaybeT(State((_: List[String]) ++ List("baz") -> Just(1)))).value.run(List("foo")),
      List("foo", "bar", "baz") -> Just(2)
    )
  }

  property("MaybeT Applicative syntax") {
    import tech.backwards.fp.learn.Applicative.syntax._

    val transformerFn: MaybeT[State[String, *], Int => Int] =
      ((x: Int) => x + 1).pure[MaybeT[State[String, *], *]]

    val transformer: MaybeT[State[String, *], Int] =
      10.pure[MaybeT[State[String, *], *]]

    assertEquals(
      transformerFn.ap(transformer).value.run("foo"),
      "foo" -> Just(11)
    )

    assertEquals(
      ((x: Int) => x + 1).pure[MaybeT[State[List[String], *], *]].ap(10.pure[MaybeT[State[List[String], *], *]]).value.run(List("foo")),
      List("foo") -> Just(11)
    )

    assertEquals(
      MaybeT(State((_: List[String]) -> Nothing[Int => Int])).ap(10.pure[MaybeT[State[List[String], *], *]]).value.run(List("foo")),
      List("foo") -> Nothing[Int]
    )

    assertEquals(
      ((x: Int) => x + 1).pure[MaybeT[State[List[String], *], *]].ap(MaybeT(State(_ -> Nothing[Int]))).value.run(List("foo")),
      List("foo") -> Nothing[Int]
    )

    assertEquals(
      MaybeT(State((_: List[String]) -> Just((x: Int) => x + 1))).ap(MaybeT(State(_ -> Just(1)))).value.run(List("foo")),
      List("foo") -> Just(2)
    )

    assertEquals(
      MaybeT(State((_: List[String]) ++ List("bar") -> Just((x: Int) => x + 1))).ap(MaybeT(State(_ ++ List("baz") -> Just(1)))).value.run(List("foo")),
      List("foo", "bar", "baz") -> Just(2)
    )
  }

  property("MaybeT Functor and Applicative syntax") {
    import tech.backwards.fp.learn.Applicative.syntax._
    import tech.backwards.fp.learn.Functor.syntax._
    import tech.backwards.fp.learn.Maybe.syntax._

    assertEquals(
      (MaybeT(State((_: String) + "bar" -> 10.just)) `<$>` (x => (y: Int) => x + y) <*> MaybeT(State(_ + "baz" -> 1.just))).value.run("foo"),
      "foobarbaz" -> 11.just
    )

    assertEquals(
      (MaybeT(State((_: List[String]) ++ List("bar") -> nothing[Int])) `<$>` (x => (y: Int) => x + y) <*> MaybeT(State(_ ++ List("baz") -> 1.just))).value.run(List("foo")),
      List("foo", "bar", "baz") -> nothing[Int]
    )

    assertEquals(
      (MaybeT(State((_: List[String]) ++List("bar") -> Just(1))) `<$>` (x => (y: Int) => x + y) <*> MaybeT(State(_ ++ List("baz") -> nothing[Int]))).value.run(List("foo")),
      List("foo", "bar", "baz") -> nothing[Int]
    )

    val add: Int => Int => Int =
      x => y => x + y

    assertEquals(
      (MaybeT(State((_: List[String]) ++ List("bar") -> 10.just)) `<$>` add <*> MaybeT(State(_ ++ List("baz") -> nothing[Int]))).value.run(List("foo")),
      List("foo", "bar", "baz") -> nothing[Int]
    )

    assertEquals(
      (MaybeT(State((_: List[String]) ++ List("bar") -> 10.just)) `<$>` add <*> MaybeT(State(_ ++ List("baz") -> 1.just))).value.run(List("foo")),
      List("foo", "bar", "baz") -> Just(11)
    )
  }

  property("MaybeT Functor and Applicative function syntax") {
    import tech.backwards.fp.learn.Applicative.syntax._
    import tech.backwards.fp.learn.Functor.syntax.function._
    import tech.backwards.fp.learn.Maybe.syntax._

    assertEquals(
      (((x: Int) => (y: Int) => x + y) `<$>` MaybeT(State((_: String) + "bar" -> 10.just)) <*> MaybeT(State(_ + "baz" -> 1.just))).value.run("foo"),
      "foobarbaz" -> 11.just
    )

    assertEquals(
      (((x: Int) => (y: Int) => x + y) `<$>` MaybeT(State((_: List[String]) ++ List("bar") -> nothing[Int])) <*> MaybeT(State(_ ++ List("baz") -> 1.just))).value.run(List("foo")),
      List("foo", "bar", "baz") -> nothing[Int]
    )

    assertEquals(
      (((x: Int) => (y: Int) => x + y) `<$>` MaybeT(State((_: List[String]) ++ List("bar") -> 10.just)) <*> MaybeT(State(_ ++ List("baz") -> nothing[Int]))).value.run(List("foo")),
      List("foo", "bar", "baz") -> nothing[Int]
    )

    val add: Int => Int => Int =
      x => y => x + y

    assertEquals(
      (add `<$>` MaybeT(State((_: List[String]) ++ List("bar") -> 10.just)) <*> MaybeT(State(_ ++ List("baz") -> 1.just))).value.run(List("foo")),
      List("foo", "bar", "baz") -> 11.just
    )

    assertEquals(
      (add `<$>` MaybeT(State((_: List[String]) ++ List("bar") -> 10.just)) <*> MaybeT(State(_ ++ List("baz") -> nothing[Int]))).value.run(List("foo")),
      List("foo", "bar", "baz") -> nothing[Int]
    )
  }

  property("MaybeT for comprehension") {
    import tech.backwards.fp.learn.Functor.syntax._
    import tech.backwards.fp.learn.Maybe.syntax._
    import tech.backwards.fp.learn.Monad.syntax._

    val transformer: MaybeT[State[String, *], Int] =
      for {
        x <- 10.pure[MaybeT[State[String, *], *]]
        y <- 11.pure[MaybeT[State[String, *], *]]
        z <- 12.pure[MaybeT[State[String, *], *]]
      } yield x + y + z

    assertEquals(
      transformer.value.run("foo"),
      "foo" -> Just(33)
    )

    assertEquals(
      (
        for {
          x <- MaybeT.lift(State((_: List[String]) ++ List("foo") -> 10))
          y <- MaybeT.lift(State((_: List[String]) ++ List("bar") -> 11))
          z <- MaybeT.lift(State((_: List[String]) ++ List("baz") -> 12))
          _ <- MaybeT(State((_: List[String]) ++ List("blah") -> 0).map(Just.apply)) // Without "lift"
        } yield x + y + z
      ).value.run(List("boo")),
      List("boo", "foo", "bar", "baz", "blah") -> Just(33)
    )

    assertEquals(
      (
        for {
          x <- MaybeT.lift(State((_: List[String]) ++ List("foo") -> 10))
          y <- MaybeT.lift(State((_: List[String]) ++ List("bar") -> 11))
          z <- MaybeT.lift(State((_: List[String]) ++ List("baz") -> 12))
          _ <- MaybeT(State((_: List[String]) ++ List("blah") -> 0).map(_ => Nothing[Int])) // Without "lift"
        } yield x + y + z
      ).value.run(List("boo")),
      List("boo", "foo", "bar", "baz", "blah") -> Nothing[Int]
    )

    assertEquals(
      (
        for {
          x <- MaybeT(State((_: List[String]) ++ List("foo") -> nothing[Int]))
          y <- MaybeT.lift(State((_: List[String]) ++ List("bar") -> 11))
          z <- MaybeT.lift(State((_: List[String]) ++ List("baz") -> 12))
          _ <- MaybeT(State((_: List[String]) ++ List("blah") -> 0).map(Just.apply)) // Without "lift"
        } yield x + y + z
      ).value.run(List("boo")),
      List("boo", "foo") -> nothing[Int]
    )
  }
}