package tech.backwards.fp.learn

import munit.ScalaCheckSuite
import org.scalacheck.Test

class MaybeMonadTransformerWriterSuite extends ScalaCheckSuite {
  override protected def scalaCheckTestParameters: Test.Parameters =
    super.scalaCheckTestParameters.withMinSuccessfulTests(100).withMaxDiscardRatio(10)

  property("MaybeT") {
    val transformer: MaybeT[Writer[String, *], Int] =
      MaybeT(Writer("" -> Just(10)))

    assertEquals(
      transformer.value.run(),
      "" -> Just(10)
    )

    assertEquals(
      MaybeT(Writer(Nil -> Nothing[Int])).value.run(),
      Nil -> Nothing[Int]
    )
  }

  property("MaybeT pure") {
    val transformer: MaybeT[Writer[String, *], Int] =
      MaybeT.pure[Writer[String, *], Int](10)

    assertEquals(
      transformer.value.run(),
      "" -> Just(10)
    )

    assertEquals(
      MaybeT.pure[Writer[List[String], *], Int](10).value.run(),
      Nil -> Just(10)
    )
  }

  property("MaybeT lift") {
    val transformer: MaybeT[Writer[String, *], Int] =
      MaybeT.lift(Writer("" -> 10))

    assertEquals(
      transformer.value.run(),
      "" -> Just(10)
    )

    assertEquals(
      MaybeT.lift(Writer(Nil -> 10)).value.run(),
      Nil -> Just(10)
    )
  }

  property("MaybeT Functor") {
    val transformer: MaybeT[Writer[String, *], Int] =
      MaybeT(Writer("" -> Just(10)))

    assertEquals(
      Functor[MaybeT[Writer[String, *], *]].fmap(transformer)(_ + 1).value.run(),
      "" -> Just(11)
    )

    assertEquals(
      Functor[MaybeT[Writer[List[String], *], *]].fmap(MaybeT(Writer(Nil -> Just(10))))(_ + 1).value.run(),
      Nil -> Just(11)
    )

    assertEquals(
      Functor[MaybeT[Writer[List[String], *], *]].fmap(MaybeT(Writer(Nil -> Nothing[Int])))(_ + 1).value.run(),
      Nil -> Nothing[Int]
    )
  }

  property("MaybeT Functor syntax") {
    import tech.backwards.fp.learn.Functor.syntax._
    import tech.backwards.fp.learn.Maybe.syntax._

    val transformer: MaybeT[Writer[String, *], Int] =
      MaybeT(Writer("" -> 10.just))

    assertEquals(
      transformer.fmap(_ + 1).value.run(),
      "" -> 11.just
    )

    assertEquals(
      MaybeT(Writer(Nil -> 10.just)).fmap(_ + 1).value.run(),
      Nil -> 11.just
    )

    assertEquals(
      MaybeT(Writer(Nil -> Nothing[Int])).fmap(_ + 1).value.run(),
      Nil -> Nothing[Int]
    )
  }

  property("MaybeT Monad") {
    val transformer: MaybeT[Writer[String, *], Int] =
      Monad[MaybeT[Writer[String, *], *]].pure(10)

    assertEquals(
      Monad[MaybeT[Writer[String, *], *]].flatMap(transformer)(a => Monad[MaybeT[Writer[String, *], *]].pure(a + 1)).value.run(),
      "" -> Just(11)
    )

    assertEquals(
      Monad[MaybeT[Writer[List[String], *], *]].flatMap(Monad[MaybeT[Writer[List[String], *], *]].pure(10))(a => Monad[MaybeT[Writer[List[String], *], *]].pure(a + 1)).value.run(),
      Nil -> Just(11)
    )

    assertEquals(
      Monad[MaybeT[Writer[List[String], *], *]].flatMap(MaybeT(Writer(Nil -> Nothing[Int])))(a => Monad[MaybeT[Writer[List[String], *], *]].pure(a + 1)).value.run(),
      Nil -> Nothing[Int]
    )

    assertEquals(
      Monad[MaybeT[Writer[List[String], *], *]].flatMap(Monad[MaybeT[Writer[List[String], *], *]].pure(10))(_ => MaybeT(Writer(Nil -> Nothing[Int]))).value.run(),
      Nil -> Nothing[Int]
    )

    assertEquals(
      Monad[MaybeT[Writer[List[String], *], *]].flatMap(MaybeT(Writer(Nil -> Just(1))))(a => MaybeT(Writer(Nil -> Just(a + 1)))).value.run(),
      Nil -> Just(2)
    )

    assertEquals(
      Monad[MaybeT[Writer[List[String], *], *]].flatMap(MaybeT(Writer(List("foo") -> Just(1))))(a => MaybeT(Writer(List("bar") -> Just(a + 1)))).value.run(),
      List("foo", "bar") -> Just(2)
    )
  }

  property("MaybeT Monad syntax") {
    import tech.backwards.fp.learn.Maybe.syntax._
    import tech.backwards.fp.learn.Monad.syntax._

    val transformer: MaybeT[Writer[String, *], Int] =
      10.pure[MaybeT[Writer[String, *], *]]

    assertEquals(
      transformer.flatMap(a => (a + 1).pure[MaybeT[Writer[String, *], *]]).value.run(),
      "" -> 11.just
    )

    assertEquals(
      10.pure[MaybeT[Writer[List[String], *], *]].flatMap(a => (a + 1).pure[MaybeT[Writer[List[String], *], *]]).value.run(),
      Nil -> 11.just
    )

    assertEquals(
      MaybeT(Writer(List.empty[String] -> nothing[Int])).flatMap(a => (a + 1).pure[MaybeT[Writer[List[String], *], *]]).value.run(),
      Nil -> nothing[Int]
    )

    assertEquals(
      10.pure[MaybeT[Writer[List[String], *], *]].flatMap(_ => MaybeT(Writer(Nil -> nothing[Int]))).value.run(),
      Nil -> nothing[Int]
    )


    assertEquals(
      MaybeT(Writer(List.empty[String] -> Just(1))).flatMap(a => MaybeT(Writer(Nil -> Just(a + 1)))).value.run(),
      Nil -> Just(2)
    )

    assertEquals(
      MaybeT(Writer(List("foo") -> Just(1))).flatMap(a => MaybeT(Writer(List("bar") -> Just(a + 1)))).value.run(),
      List("foo", "bar") -> Just(2)
    )
  }

  property("MaybeT Applicative") {
    val transformerFn: MaybeT[Writer[String, *], Int => Int] =
      Applicative[MaybeT[Writer[String, *], *]].pure(_ + 1)

    val transformer: MaybeT[Writer[String, *], Int] =
      Applicative[MaybeT[Writer[String, *], *]].pure(10)

    assertEquals(
      Applicative[MaybeT[Writer[String, *], *]].ap(transformerFn)(transformer).value.run(),
      "" -> Just(11)
    )

    assertEquals(
      Applicative[MaybeT[Writer[List[String], *], *]].ap(Applicative[MaybeT[Writer[List[String], *], *]].pure((x: Int) => x + 1))(Applicative[MaybeT[Writer[List[String], *], *]].pure(10)).value.run(),
      Nil -> Just(11)
    )

    assertEquals(
      Applicative[MaybeT[Writer[List[String], *], *]].ap(MaybeT(Writer(Nil -> Nothing[Int => Int])))(Applicative[MaybeT[Writer[List[String], *], *]].pure(10)).value.run(),
      Nil -> Nothing[Int]
    )

    assertEquals(
      Applicative[MaybeT[Writer[List[String], *], *]].ap(Applicative[MaybeT[Writer[List[String], *], *]].pure((x: Int) => x + 1))(MaybeT(Writer(Nil -> Nothing[Int]))).value.run(),
      Nil -> Nothing[Int]
    )

    assertEquals(
      Applicative[MaybeT[Writer[List[String], *], *]].ap(MaybeT(Writer(Nil -> Just((x: Int) => x + 1))))(MaybeT(Writer(Nil -> Nothing[Int]))).value.run(),
      Nil -> Nothing[Int]
    )

    assertEquals(
      Applicative[MaybeT[Writer[List[String], *], *]].ap(MaybeT(Writer(Nil -> Just((x: Int) => x + 1))))(MaybeT(Writer(Nil -> Just(1)))).value.run(),
      Nil -> Just(2)
    )

    assertEquals(
      Applicative[MaybeT[Writer[List[String], *], *]].ap(MaybeT(Writer(List("foo") -> Just((x: Int) => x + 1))))(MaybeT(Writer(List("bar") -> Just(1)))).value.run(),
      List("foo", "bar") -> Just(2)
    )
  }

  property("MaybeT Applicative syntax") {
    import tech.backwards.fp.learn.Applicative.syntax._

    val transformerFn: MaybeT[Writer[String, *], Int => Int] =
      ((x: Int) => x + 1).pure[MaybeT[Writer[String, *], *]]

    val transformer: MaybeT[Writer[String, *], Int] =
      10.pure[MaybeT[Writer[String, *], *]]

    assertEquals(
      transformerFn.ap(transformer).value.run(),
      "" -> Just(11)
    )

    assertEquals(
      ((x: Int) => x + 1).pure[MaybeT[Writer[List[String], *], *]].ap(10.pure[MaybeT[Writer[List[String], *], *]]).value.run(),
      Nil -> Just(11)
    )

    assertEquals(
      MaybeT(Writer(List.empty[String] -> Nothing[Int => Int])).ap(10.pure[MaybeT[Writer[List[String], *], *]]).value.run(),
      Nil -> Nothing[Int]
    )

    assertEquals(
      ((x: Int) => x + 1).pure[MaybeT[Writer[List[String], *], *]].ap(MaybeT(Writer(Nil -> Nothing[Int]))).value.run(),
      Nil -> Nothing[Int]
    )

    assertEquals(
      MaybeT(Writer(List.empty[String] -> Just((x: Int) => x + 1))).ap(MaybeT(Writer(Nil -> Just(1)))).value.run(),
      Nil -> Just(2)
    )

    assertEquals(
      MaybeT(Writer(List("foo") -> Just((x: Int) => x + 1))).ap(MaybeT(Writer(List("bar") -> Just(1)))).value.run(),
      List("foo", "bar") -> Just(2)
    )
  }

  property("MaybeT Functor and Applicative syntax") {
    import tech.backwards.fp.learn.Applicative.syntax._
    import tech.backwards.fp.learn.Functor.syntax._
    import tech.backwards.fp.learn.Maybe.syntax._

    assertEquals(
      (MaybeT(Writer("foo" -> 10.just)) `<$>` (x => (y: Int) => x + y) <*> MaybeT(Writer("bar" -> 1.just))).value.run(),
      "foobar" -> 11.just
    )

    assertEquals(
      (MaybeT(Writer(List("foo") -> nothing[Int])) `<$>` (x => (y: Int) => x + y) <*> MaybeT(Writer(List("bar") -> 1.just))).value.run(),
      List("foo", "bar") -> nothing[Int]
    )

    assertEquals(
      (MaybeT(Writer(List("foo") -> Just(1))) `<$>` (x => (y: Int) => x + y) <*> MaybeT(Writer(List("bar") -> nothing[Int]))).value.run(),
      List("foo", "bar") -> nothing[Int]
    )

    val add: Int => Int => Int =
      x => y => x + y

    assertEquals(
      (MaybeT(Writer(List("foo") -> 10.just)) `<$>` add <*> MaybeT(Writer(List("bar") -> nothing[Int]))).value.run(),
      List("foo", "bar") -> nothing[Int]
    )

    assertEquals(
      (MaybeT(Writer(List("foo") -> 10.just)) `<$>` add <*> MaybeT(Writer(List("bar") -> Just(1)))).value.run(),
      List("foo", "bar") -> Just(11)
    )
  }

  property("MaybeT Functor and Applicative function syntax") {
    import tech.backwards.fp.learn.Applicative.syntax._
    import tech.backwards.fp.learn.Functor.syntax.function._
    import tech.backwards.fp.learn.Maybe.syntax._

    assertEquals(
      (((x: Int) => (y: Int) => x + y) `<$>` MaybeT(Writer("foo" -> 10.just)) <*> MaybeT(Writer("bar" -> 1.just))).value.run(),
      "foobar" -> 11.just
    )

    assertEquals(
      (((x: Int) => (y: Int) => x + y) `<$>` MaybeT(Writer(List("foo") -> nothing[Int])) <*> MaybeT(Writer(List("bar") -> 1.just))).value.run(),
      List("foo", "bar") -> nothing[Int]
    )

    assertEquals(
      (((x: Int) => (y: Int) => x + y) `<$>` MaybeT(Writer(List("foo") -> 10.just)) <*> MaybeT(Writer(List("bar") -> nothing[Int]))).value.run(),
      List("foo", "bar") -> nothing[Int]
    )

    val add: Int => Int => Int =
      x => y => x + y

    assertEquals(
      (add `<$>` MaybeT(Writer(List("foo") -> 10.just)) <*> MaybeT(Writer(List("bar") -> 1.just))).value.run(),
      List("foo", "bar") -> 11.just
    )

    assertEquals(
      (add `<$>` MaybeT(Writer(List("foo") -> 10.just)) <*> MaybeT(Writer(List("bar") -> nothing[Int]))).value.run(),
      List("foo", "bar") -> nothing[Int]
    )
  }

  property("MaybeT for comprehension") {
    import tech.backwards.fp.learn.Functor.syntax._
    import tech.backwards.fp.learn.Maybe.syntax._
    import tech.backwards.fp.learn.Monad.syntax._

    val transformer: MaybeT[Writer[String, *], Int] =
      for {
        x <- 10.pure[MaybeT[Writer[String, *], *]]
        y <- 11.pure[MaybeT[Writer[String, *], *]]
        z <- 12.pure[MaybeT[Writer[String, *], *]]
      } yield x + y + z

    assertEquals(
      transformer.value.run(),
      "" -> Just(33)
    )

    assertEquals(
      (
        for {
          x <- MaybeT.lift(Writer(List("foo") -> 10))
          y <- MaybeT.lift(Writer(List("bar") -> 11))
          z <- MaybeT.lift(Writer(List("baz") -> 12))
          _ <- MaybeT(Writer(List("blah") -> 0).map(Just.apply)) // Without "lift"
        } yield x + y + z
      ).value.run(),
      List("foo", "bar", "baz", "blah") -> Just(33)
    )

    assertEquals(
      (
        for {
          x <- MaybeT.lift(Writer(List("foo") -> 10))
          y <- MaybeT.lift(Writer(List("bar") -> 11))
          z <- MaybeT.lift(Writer(List("baz") -> 12))
          _ <- MaybeT(Writer(List("blah") -> 0).map(_ => Nothing[Int])) // Without "lift"
        } yield x + y + z
      ).value.run(),
      List("foo", "bar", "baz", "blah") -> Nothing[Int]
    )

    assertEquals(
      (
        for {
          x <- MaybeT(Writer(List("foo") -> nothing[Int]))
          y <- MaybeT.lift(Writer(List("bar") -> 11))
          z <- MaybeT.lift(Writer(List("baz") -> 12))
          _ <- MaybeT(Writer(List("blah") -> 0).map(Just.apply)) // Without "lift"
        } yield x + y + z
      ).value.run(),
      List("foo") -> nothing[Int]
    )
  }
}