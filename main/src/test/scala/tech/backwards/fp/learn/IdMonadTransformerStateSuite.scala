package tech.backwards.fp.learn

import munit.ScalaCheckSuite
import org.scalacheck.Test

class IdMonadTransformerStateSuite extends ScalaCheckSuite {
  override protected def scalaCheckTestParameters: Test.Parameters =
    super.scalaCheckTestParameters.withMinSuccessfulTests(100).withMaxDiscardRatio(10)

  property("IdT") {
    val transformer: IdT[State[String, *], Int] =
      IdT(State((s: String) => s"${s}bar" -> Id(10)))

    assertEquals(
      transformer.value.run("foo"),
      "foobar" -> Id(10)
    )
  }

  property("IdT pure") {
    val transformer: IdT[State[String, *], Int] =
      IdT.pure[State[String, *], Int](10)

    assertEquals(
      transformer.value.run("foo"),
      "foo" -> Id(10)
    )
  }

  property("IdT lift") {
    val transformer: IdT[State[String, *], Int] =
      IdT.lift(State((s: String) => s"${s}bar" -> 10))

    assertEquals(
      transformer.value.run("foo"),
      "foobar" -> Id(10)
    )

    assertEquals(
      IdT.lift(State((s: String) => s"${s}bar" -> 10)).value.run("foo"),
      "foobar" -> Id(10)
    )
  }

  property("IdT Functor") {
    val transformer: IdT[State[String, *], Int] =
      IdT(State((s: String) => s"${s}bar" -> Id(10)))

    assertEquals(
      Functor[IdT[State[String, *], *]].fmap(transformer)(_ + 1).value.run("foo"),
      "foobar" -> Id(11)
    )

    assertEquals(
      Functor[IdT[State[String, *], *]].fmap(IdT(State((s: String) => s"${s}bar" -> Id(10))))(_ + 1).value.run("foo"),
      "foobar" -> Id(11)
    )
  }

  property("IdT Functor syntax") {
    import tech.backwards.fp.learn.Functor.syntax._

    val transformer: IdT[State[String, *], Int] =
      IdT(State((s: String) => s"${s}bar" -> Id(10)))

    assertEquals(
      transformer.fmap(_ + 1).value.run("foo"),
      "foobar" -> Id(11)
    )

    assertEquals(
      IdT(State((s: String) => s"${s}bar" -> Id(10))).fmap(_ + 1).value.run("foo"),
      "foobar" -> Id(11)
    )

    assertEquals(
      (IdT(State((s: String) => s"${s}bar" -> Id(10))) `<$>` (_ + 1)).value.run("foo"),
      "foobar" -> Id(11)
    )
  }

  property("IdT Monad") {
    val transformer: IdT[State[String, *], Int] =
      Monad[IdT[State[String, *], *]].pure(10)

    assertEquals(
      Monad[IdT[State[String, *], *]].flatMap(transformer)(a => Monad[IdT[State[String, *], *]].pure(a + 1)).value.run("foo"),
      "foo" -> Id(11)
    )

    assertEquals(
      Monad[IdT[State[String, *], *]].flatMap(Monad[IdT[State[String, *], *]].pure(10))(a => Monad[IdT[State[String, *], *]].pure(a + 1)).value.run("foo"),
      "foo" -> Id(11)
    )
  }

  property("IdT Monad syntax") {
    import tech.backwards.fp.learn.Monad.syntax._

    val transformer: IdT[State[String, *], Int] =
      10.pure[IdT[State[String, *], *]]

    assertEquals(
      transformer.flatMap(a => (a + 1).pure[IdT[State[String, *], *]]).value.run("foo"),
      "foo" -> Id(11)
    )

    assertEquals(
      10.pure[IdT[State[String, *], *]].flatMap(a => (a + 1).pure[IdT[State[String, *], *]]).value.run("foo"),
      "foo" -> Id(11)
    )

    assertEquals(
      10.pure[IdT[State[String, *], *]].flatMap(a => IdT(State(_ -> Id(a + 1)))).value.run("foo"),
      "foo" -> Id(11)
    )
  }

  property("IdT Applicative") {
    val transformerFn: IdT[State[String, *], Int => Int] =
      Applicative[IdT[State[String, *], *]].pure(_ + 1)

    val transformer: IdT[State[String, *], Int] =
      Applicative[IdT[State[String, *], *]].pure(10)

    assertEquals(
      Applicative[IdT[State[String, *], *]].ap(transformerFn)(transformer).value.run("foo"),
      "foo" -> Id(11)
    )

    assertEquals(
      Applicative[IdT[State[String, *], *]].ap(Applicative[IdT[State[String, *], *]].pure((x: Int) => x + 1))(Applicative[IdT[State[String, *], *]].pure(10)).value.run("foo"),
      "foo" -> Id(11)
    )
  }

  property("IdT Applicative syntax") {
    import tech.backwards.fp.learn.Applicative.syntax._

    val transformerFn: IdT[State[String, *], Int => Int] =
      ((x: Int) => x + 1).pure[IdT[State[String, *], *]]

    val transformer: IdT[State[String, *], Int] =
      10.pure[IdT[State[String, *], *]]

    assertEquals(
      transformerFn.ap(transformer).value.run("foo"),
      "foo" -> Id(11)
    )

    assertEquals(
      ((x: Int) => x + 1).pure[IdT[State[String, *], *]].ap(10.pure[IdT[State[String, *], *]]).value.run("foo"),
      "foo" -> Id(11)
    )

    assertEquals(
      10.pure[IdT[State[String, *], *]].ap(((x: Int) => x + 1).pure[IdT[State[String, *], *]]).value.run("foo"),
      "foo" -> Id(11)
    )
  }

  property("IdT Functor and Applicative syntax") {
    import tech.backwards.fp.learn.Applicative.syntax._
    import tech.backwards.fp.learn.Functor.syntax._

    assertEquals(
      (IdT(State((s: String) => s"${s}bar" -> Id(10))) `<$>` (x => (y: Int) => x + y) <*> IdT(State((s: String) => s"${s}baz" -> Id(1)))).value.run("foo"),
      "foobarbaz" -> Id(11)
    )

    val add: Int => Int => Int =
      x => y => x + y

    assertEquals(
      (IdT(State((s: String) => s"${s}bar" -> Id(10))) `<$>` add <*> IdT(State((s: String) => s"${s}baz" -> Id(1)))).value.run("foo"),
      "foobarbaz" -> Id(11)
    )
  }

  property("IdT Functor and Applicative function syntax") {
    import tech.backwards.fp.learn.Applicative.syntax._
    import tech.backwards.fp.learn.Functor.syntax.function._

    assertEquals(
      (((x: Int) => (y: Int) => x + y) `<$>` IdT(State((s: String) => s"${s}bar" -> Id(10))) <*> IdT(State((s: String) => s"${s}baz" -> Id(1)))).value.run("foo"),
      "foobarbaz" -> Id(11)
    )

    val add: Int => Int => Int =
      x => y => x + y

    assertEquals(
      (add `<$>` IdT(State((s: String) => s"${s}bar" -> Id(10))) <*> IdT(State((s: String) => s"${s}baz" -> Id(1)))).value.run("foo"),
      "foobarbaz" -> Id(11)
    )
  }

  property("IdT for comprehension") {
    import tech.backwards.fp.learn.Functor.syntax._
    import tech.backwards.fp.learn.Monad.syntax._

    val transformer: IdT[State[String, *], Int] =
      for {
        x <- 10.pure[IdT[State[String, *], *]]
        y <- 11.pure[IdT[State[String, *], *]]
        z <- 12.pure[IdT[State[String, *], *]]
      } yield x + y + z

    assertEquals(
      transformer.value.run("foo"),
      "foo" -> Id(33)
    )

    assertEquals(
      (for {
        x <- IdT.lift(State((s: String) => s"${s}bar" -> 10))
        y <- IdT.lift(State((s: String) => s"${s}baz" -> 11))
        z <- IdT.lift(State((s: String) => s"$s-and-then-some" -> 12))
        _ <- IdT(State((s: String) => s"$s-without-lift" -> Id(0))) // Without "lift"
      } yield x + y + z).value.run("foo"),
      "foobarbaz-and-then-some-without-lift" -> Id(33)
    )
  }
}