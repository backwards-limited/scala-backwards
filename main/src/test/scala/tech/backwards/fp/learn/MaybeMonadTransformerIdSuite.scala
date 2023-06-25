package tech.backwards.fp.learn

import munit.ScalaCheckSuite
import org.scalacheck.Test

/**
 * First of the following MaybeT suites:
 *  - MaybeMonadTransformerMaybeSuite
 *  - MaybeMonadTransformerDisjunctionSuite
 *  - MaybeMonadTransformerListSuite
 *  - MaybeMonadTransformerStateSuite
 *  - MaybeMonadTransformerWriterSuite
 */
class MaybeMonadTransformerIdSuite extends ScalaCheckSuite {
  override protected def scalaCheckTestParameters: Test.Parameters =
    super.scalaCheckTestParameters.withMinSuccessfulTests(100).withMaxDiscardRatio(10)

  property("MaybeT") {
    val transformer: MaybeT[Id, Int] =
      MaybeT(Id(Just(10)))

    assertEquals(
      transformer.value,
      Id(Just(10))
    )
  }

  property("MaybeT pure") {
    val transformer: MaybeT[Id, Int] =
      MaybeT.pure[Id, Int](10)

    assertEquals(
      transformer.value,
      Id(Just(10))
    )
  }

  property("MaybeT lift") {
    val transformer: MaybeT[Id, Int] =
      MaybeT.lift(Id(10))

    assertEquals(
      transformer.value,
      Id(Just(10))
    )

    assertEquals(
      MaybeT.lift(Id(10)).value,
      Id(Just(10))
    )
  }

  property("MaybeT Functor") {
    val transformer: MaybeT[Id, Int] =
      MaybeT(Id(Just(10)))

    assertEquals(
      Functor[MaybeT[Id, *]].fmap(transformer)(_ + 1).value,
      Id(Just(11))
    )

    assertEquals(
      Functor[MaybeT[Id, *]].fmap(MaybeT(Id(Just(10))))(_ + 1).value,
      Id(Just(11))
    )

    assertEquals(
      Functor[MaybeT[Id, *]].fmap(MaybeT(Id(Nothing[Int])))(_ + 1).value,
      Id(Nothing[Int])
    )
  }

  property("MaybeT Functor syntax") {
    import tech.backwards.fp.learn.Functor.syntax._
    import tech.backwards.fp.learn.Maybe.syntax._

    val transformer: MaybeT[Id, Int] =
      MaybeT(Id(10.just))

    assertEquals(
      transformer.fmap(_ + 1).value,
      Id(11.just)
    )

    assertEquals(
      MaybeT(Id(10.just)).fmap(_ + 1).value,
      Id(11.just)
    )

    assertEquals(
      MaybeT(Id(10.just)) fmap (_ + 1),
      MaybeT(Id(11.just))
    )

    assertEquals(
      MaybeT(Id(10.just)) `<$>` (_ + 1),
      MaybeT(Id(11.just))
    )

    assertEquals(
      MaybeT(Id(nothing[Int])) `<$>` (_ + 1),
      MaybeT(Id(nothing[Int]))
    )
  }

  property("MaybeT Monad") {
    val transformer: MaybeT[Id, Int] =
      Monad[MaybeT[Id, *]].pure(10)

    assertEquals(
      Monad[MaybeT[Id, *]].flatMap(transformer)(a => Monad[MaybeT[Id, *]].pure(a + 1)).value,
      Id(Just(11))
    )

    assertEquals(
      Monad[MaybeT[Id, *]].flatMap(Monad[MaybeT[Id, *]].pure(10))(a => Monad[MaybeT[Id, *]].pure(a + 1)).value,
      Id(Just(11))
    )

    assertEquals(
      Monad[MaybeT[Id, *]].flatMap(MaybeT(Id(Nothing[Int])))(a => Monad[MaybeT[Id, *]].pure(a + 1)).value,
      Id(Nothing[Int])
    )

    assertEquals(
      Monad[MaybeT[Id, *]].flatMap(Monad[MaybeT[Id, *]].pure(10))(_ => MaybeT(Id(Nothing[Int]))).value,
      Id(Nothing[Int])
    )

    assertEquals(
      Monad[MaybeT[Id, *]].flatMap(MaybeT(Id(Nothing[Int])))(_ => MaybeT(Id(Nothing[Int]))).value,
      Id(Nothing[Int])
    )
  }

  property("MaybeT Monad syntax") {
    import tech.backwards.fp.learn.Monad.syntax._

    val transformer: MaybeT[Id, Int] =
      10.pure[MaybeT[Id, *]]

    assertEquals(
      transformer.flatMap(a => (a + 1).pure[MaybeT[Id, *]]).value,
      Id(Just(11))
    )

    assertEquals(
      10.pure[MaybeT[Id, *]].flatMap(a => (a + 1).pure[MaybeT[Id, *]]).value,
      Id(Just(11))
    )

    assertEquals(
      MaybeT(Id(Nothing[Int])).flatMap(a => (a + 1).pure[MaybeT[Id, *]]).value,
      Id(Nothing[Int])
    )

    assertEquals(
      10.pure[MaybeT[Id, *]].flatMap(_ => MaybeT(Id(Nothing[Int]))).value,
      Id(Nothing[Int])
    )

    assertEquals(
      MaybeT(Id(Nothing[Int])).flatMap(_ => MaybeT(Id(Nothing[Int]))).value,
      Id(Nothing[Int])
    )
  }

  property("MaybeT Applicative") {
    val transformerFn: MaybeT[Id, Int => Int] =
      Applicative[MaybeT[Id, *]].pure(_ + 1)

    val transformer: MaybeT[Id, Int] =
      Applicative[MaybeT[Id, *]].pure(10)

    assertEquals(
      Applicative[MaybeT[Id, *]].ap(transformerFn)(transformer).value,
      Id(Just(11))
    )

    assertEquals(
      Applicative[MaybeT[Id, *]].ap(Applicative[MaybeT[Id, *]].pure((x: Int) => x + 1))(Applicative[MaybeT[Id, *]].pure(10)).value,
      Id(Just(11))
    )

    assertEquals(
      Applicative[MaybeT[Id, *]].ap(MaybeT(Id(Nothing[Int => Int])))(Applicative[MaybeT[Id, *]].pure(10)).value,
      Id(Nothing[Int])
    )

    assertEquals(
      Applicative[MaybeT[Id, *]].ap(Applicative[MaybeT[Id, *]].pure((x: Int) => x + 1))(MaybeT(Id(Nothing[Int]))).value,
      Id(Nothing[Int])
    )
  }

  property("MaybeT Applicative sytax") {
    import tech.backwards.fp.learn.Applicative.syntax._

    val transformerFn: MaybeT[Id, Int => Int] =
      ((x: Int) => x + 1).pure[MaybeT[Id, *]]

    val transformer: MaybeT[Id, Int] =
      10.pure[MaybeT[Id, *]]

    assertEquals(
      transformerFn.ap(transformer).value,
      Id(Just(11))
    )

    assertEquals(
      ((x: Int) => x + 1).pure[MaybeT[Id, *]].ap(10.pure[MaybeT[Id, *]]).value,
      Id(Just(11))
    )

    assertEquals(
      MaybeT(Id(Nothing[Int => Int])).ap(10.pure[MaybeT[Id, *]]).value,
      Id(Nothing[Int])
    )

    assertEquals(
      ((x: Int) => x + 1).pure[MaybeT[Id, *]].ap(MaybeT(Id(Nothing[Int]))).value,
      Id(Nothing[Int])
    )

    assertEquals(
      ((x: Int) => x + 1).pure[MaybeT[Id, *]] <*> MaybeT(Id(Nothing[Int])),
      MaybeT(Id(Nothing[Int]))
    )
  }

  property("MaybeT Functor and Applicative syntax") {
    import tech.backwards.fp.learn.Applicative.syntax._
    import tech.backwards.fp.learn.Functor.syntax._
    import tech.backwards.fp.learn.Maybe.syntax._

    assertEquals(
      MaybeT(Id(Just(10))) `<$>` (x => (y: Int) => x + y) <*> MaybeT(Id(Just(1))),
      MaybeT(Id(Just(11)))
    )

    assertEquals(
      MaybeT(Id(nothing[Int])) `<$>` (x => (y: Int) => x + y) <*> MaybeT(Id(Just(1))),
      MaybeT(Id(nothing[Int]))
    )

    assertEquals(
      MaybeT(Id(Just(10))) `<$>` (x => (y: Int) => x + y) <*> MaybeT(Id(nothing[Int])),
      MaybeT(Id(nothing[Int]))
    )

    val add: Int => Int => Int =
      x => y => x + y

    assertEquals(
      MaybeT(Id(Just(10))) `<$>` add <*> MaybeT(Id(nothing[Int])),
      MaybeT(Id(nothing[Int]))
    )
  }

  property("MaybeT Functor and Applicative function syntax") {
    import tech.backwards.fp.learn.Applicative.syntax._
    import tech.backwards.fp.learn.Functor.syntax.function._
    import tech.backwards.fp.learn.Maybe.syntax._

    assertEquals(
      ((x: Int) => (y: Int) => x + y) `<$>` MaybeT(Id(Just(10))) <*> MaybeT(Id(Just(1))),
      MaybeT(Id(Just(11)))
    )

    assertEquals(
      ((x: Int) => (y: Int) => x + y) `<$>` MaybeT(Id(nothing[Int])) <*> MaybeT(Id(Just(1))),
      MaybeT(Id(nothing[Int]))
    )

    assertEquals(
      ((x: Int) => (y: Int) => x + y) `<$>` MaybeT(Id(Just(10))) <*> MaybeT(Id(nothing[Int])),
      MaybeT(Id(nothing[Int]))
    )

    val add: Int => Int => Int =
      x => y => x + y

    assertEquals(
      add `<$>` MaybeT(Id(Just(10))) <*> MaybeT(Id(Just(1))),
      MaybeT(Id(Just(11)))
    )
  }

  property("MaybeT for comprehension") {
    import tech.backwards.fp.learn.Functor.syntax._
    import tech.backwards.fp.learn.Maybe.syntax._
    import tech.backwards.fp.learn.Monad.syntax._

    val transformer: MaybeT[Id, Int] =
      for {
        x <- 10.pure[MaybeT[Id, *]]
        y <- 11.pure[MaybeT[Id, *]]
        z <- 12.pure[MaybeT[Id, *]]
      } yield x + y + z

    assertEquals(
      transformer.value,
      Id(Just(33))
    )

    assertEquals(
      for {
        x <- MaybeT.lift(Id(10))
        y <- MaybeT.lift(Id(11))
        z <- MaybeT.lift(Id(12))
        _ <- MaybeT(Id(0).map(Just.apply)) // Without "lift"
      } yield x + y + z,
      MaybeT(Id(Just(33)))
    )

    assertEquals(
      for {
        x <- MaybeT.lift(Id(10))
        y <- MaybeT(Id(nothing[Int]))
        z <- MaybeT.lift(Id(12))
      } yield x + y + z,
      MaybeT(Id(nothing[Int]))
    )
  }
}