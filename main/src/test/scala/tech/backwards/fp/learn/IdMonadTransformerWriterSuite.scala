package tech.backwards.fp.learn

import munit.ScalaCheckSuite
import org.scalacheck.Test

class IdMonadTransformerWriterSuite extends ScalaCheckSuite {
  override protected def scalaCheckTestParameters: Test.Parameters =
    super.scalaCheckTestParameters.withMinSuccessfulTests(100).withMaxDiscardRatio(10)

  property("IdT") {
    val transformer: IdT[Writer[String, *], Int] =
      IdT(Writer(s"foo" -> Id(10)))

    assertEquals(
      transformer.value.run(),
      "foo" -> Id(10)
    )
  }

  property("IdT pure") {
    val transformer: IdT[Writer[String, *], Int] =
      IdT.pure[Writer[String, *], Int](10)

    assertEquals(
      transformer.value.run(),
      "" -> Id(10)
    )
  }

  property("IdT lift") {
    val transformer: IdT[Writer[String, *], Int] =
      IdT.lift(Writer(s"foo" -> 10))

    assertEquals(
      transformer.value.run(),
      "foo" -> Id(10)
    )

    assertEquals(
      IdT.lift(Writer(s"foo" -> 10)).value.run(),
      "foo" -> Id(10)
    )
  }

  property("IdT Functor") {
    val transformer: IdT[Writer[String, *], Int] =
      IdT(Writer("foo" -> Id(10)))

    assertEquals(
      Functor[IdT[Writer[String, *], *]].fmap(transformer)(_ + 1).value.run(),
      "foo" -> Id(11)
    )

    assertEquals(
      Functor[IdT[Writer[String, *], *]].fmap(IdT(Writer("foo" -> Id(10))))(_ + 1).value.run(),
      "foo" -> Id(11)
    )
  }

  property("IdT Functor syntax") {
    import tech.backwards.fp.learn.Functor.syntax._

    val transformer: IdT[Writer[String, *], Int] =
      IdT(Writer("foo" -> Id(10)))

    assertEquals(
      transformer.fmap(_ + 1).value.run(),
      "foo" -> Id(11)
    )

    assertEquals(
      IdT(Writer("foo" -> Id(10))).fmap(_ + 1).value.run(),
      "foo" -> Id(11)
    )

    assertEquals(
      (IdT(Writer("foo" -> Id(10))) `<$>` (_ + 1)).value.run(),
      "foo" -> Id(11)
    )
  }

  property("IdT Monad") {
    val transformer: IdT[Writer[String, *], Int] =
      Monad[IdT[Writer[String, *], *]].pure(10)

    assertEquals(
      Monad[IdT[Writer[String, *], *]].flatMap(transformer)(a => Monad[IdT[Writer[String, *], *]].pure(a + 1)).value.run(),
      "" -> Id(11)
    )

    assertEquals(
      Monad[IdT[Writer[String, *], *]].flatMap(Monad[IdT[Writer[String, *], *]].pure(10))(a => Monad[IdT[Writer[String, *], *]].pure(a + 1)).value.run(),
      "" -> Id(11)
    )
  }

  property("IdT Monad syntax") {
    import tech.backwards.fp.learn.Monad.syntax._

    val transformer: IdT[Writer[String, *], Int] =
      10.pure[IdT[Writer[String, *], *]]

    assertEquals(
      transformer.flatMap(a => (a + 1).pure[IdT[Writer[String, *], *]]).value.run(),
      "" -> Id(11)
    )

    assertEquals(
      10.pure[IdT[Writer[String, *], *]].flatMap(a => (a + 1).pure[IdT[Writer[String, *], *]]).value.run(),
      "" -> Id(11)
    )

    assertEquals(
      10.pure[IdT[Writer[String, *], *]].flatMap(a => IdT(Writer("foo" -> Id(a + 1)))).value.run(),
      "foo" -> Id(11)
    )
  }

  property("IdT Applicative") {
    val transformerFn: IdT[Writer[String, *], Int => Int] =
      Applicative[IdT[Writer[String, *], *]].pure(_ + 1)

    val transformer: IdT[Writer[String, *], Int] =
      Applicative[IdT[Writer[String, *], *]].pure(10)

    assertEquals(
      Applicative[IdT[Writer[String, *], *]].ap(transformerFn)(transformer).value.run(),
      "" -> Id(11)
    )

    assertEquals(
      Applicative[IdT[Writer[String, *], *]].ap(Applicative[IdT[Writer[String, *], *]].pure((x: Int) => x + 1))(Applicative[IdT[Writer[String, *], *]].pure(10)).value.run(),
      "" -> Id(11)
    )
  }

  property("IdT Applicative syntax") {
    import tech.backwards.fp.learn.Applicative.syntax._

    val transformerFn: IdT[Writer[String, *], Int => Int] =
      ((x: Int) => x + 1).pure[IdT[Writer[String, *], *]]

    val transformer: IdT[Writer[String, *], Int] =
      10.pure[IdT[Writer[String, *], *]]

    assertEquals(
      transformerFn.ap(transformer).value.run(),
      "" -> Id(11)
    )

    assertEquals(
      ((x: Int) => x + 1).pure[IdT[Writer[String, *], *]].ap(10.pure[IdT[Writer[String, *], *]]).value.run(),
      "" -> Id(11)
    )

    assertEquals(
      10.pure[IdT[Writer[String, *], *]].ap(((x: Int) => x + 1).pure[IdT[Writer[String, *], *]]).value.run(),
      "" -> Id(11)
    )
  }

  property("IdT Functor and Applicative syntax") {
    import tech.backwards.fp.learn.Applicative.syntax._
    import tech.backwards.fp.learn.Functor.syntax._

    assertEquals(
      (IdT(Writer("foo" -> Id(10))) `<$>` (x => (y: Int) => x + y) <*> IdT(Writer("bar" -> Id(1)))).value.run(),
      "foobar" -> Id(11)
    )

    val add: Int => Int => Int =
      x => y => x + y

    assertEquals(
      (IdT(Writer("foo" -> Id(10))) `<$>` add <*> IdT(Writer("bar" -> Id(1)))).value.run(),
      "foobar" -> Id(11)
    )
  }

  property("IdT Functor and Applicative function syntax") {
    import tech.backwards.fp.learn.Applicative.syntax._
    import tech.backwards.fp.learn.Functor.syntax.function._

    assertEquals(
      (((x: Int) => (y: Int) => x + y) `<$>` IdT(Writer("foo" -> Id(10))) <*> IdT(Writer("bar" -> Id(1)))).value.run(),
      "foobar" -> Id(11)
    )

    val add: Int => Int => Int =
      x => y => x + y

    assertEquals(
      (add `<$>` IdT(Writer("foo" -> Id(10))) <*> IdT(Writer("bar" -> Id(1)))).value.run(),
      "foobar" -> Id(11)
    )
  }

  property("IdT for comprehension") {
    import tech.backwards.fp.learn.Functor.syntax._
    import tech.backwards.fp.learn.Monad.syntax._

    val transformer: IdT[Writer[String, *], Int] =
      for {
        x <- 10.pure[IdT[Writer[String, *], *]]
        y <- 11.pure[IdT[Writer[String, *], *]]
        z <- 12.pure[IdT[Writer[String, *], *]]
      } yield x + y + z

    assertEquals(
      transformer.value.run(),
      "" -> Id(33)
    )

    assertEquals(
      (for {
        x <- IdT.lift(Writer("foo" -> 10))
        y <- IdT.lift(Writer("bar" -> 11))
        z <- IdT.lift(Writer("-and-then-some" -> 12))
        _ <- IdT(Writer("-without-lift" -> Id(0))) // Without "lift"
      } yield x + y + z).value.run(),
      "foobar-and-then-some-without-lift" -> Id(33)
    )
  }
}