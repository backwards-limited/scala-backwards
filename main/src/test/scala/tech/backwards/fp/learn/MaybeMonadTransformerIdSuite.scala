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

  /*property("IdT Applicative") {
    val transformerFn: IdT[Maybe, Int => Int] =
      Applicative[IdT[Maybe, *]].pure(_ + 1)

    val transformer: IdT[Maybe, Int] =
      Applicative[IdT[Maybe, *]].pure(10)

    assertEquals(
      Applicative[IdT[Maybe, *]].ap(transformerFn)(transformer).value,
      Just(Id(11))
    )

    assertEquals(
      Applicative[IdT[Maybe, *]].ap(Applicative[IdT[Maybe, *]].pure((x: Int) => x + 1))(Applicative[IdT[Maybe, *]].pure(10)).value,
      Just(Id(11))
    )

    assertEquals(
      Applicative[IdT[Maybe, *]].ap(IdT(Nothing[Id[Int => Int]]))(Applicative[IdT[Maybe, *]].pure(10)).value,
      Nothing[Id[Int]]
    )

    assertEquals(
      Applicative[IdT[Maybe, *]].ap(Applicative[IdT[Maybe, *]].pure((x: Int) => x + 1))(IdT(Nothing[Id[Int]])).value,
      Nothing[Id[Int]]
    )
  }*/

  /*property("IdT Applicative syntax") {
    import tech.backwards.fp.learn.Applicative.syntax._
    import tech.backwards.fp.learn.Maybe.syntax._

    val transformerFn: IdT[Maybe, Int => Int] =
      ((x: Int) => x + 1).pure[IdT[Maybe, *]]

    val transformer: IdT[Maybe, Int] =
      10.pure[IdT[Maybe, *]]

    assertEquals(
      transformerFn.ap(transformer).value,
      Id(11).just
    )

    assertEquals(
      ((x: Int) => x + 1).pure[IdT[Maybe, *]].ap(10.pure[IdT[Maybe, *]]).value,
      Id(11).just
    )

    assertEquals(
      ((x: Int) => x + 1).pure[IdT[Maybe, *]].ap(IdT(nothing[Id[Int]])).value,
      nothing[Id[Int]]
    )

    assertEquals(
      IdT(nothing[Id[Int => Int]]).ap(10.pure[IdT[Maybe, *]]).value,
      nothing[Id[Int]]
    )

    assertEquals(
      IdT(nothing[Id[Int => Int]]) ap 10.pure[IdT[Maybe, *]],
      IdT(nothing[Id[Int]])
    )

    assertEquals(
      IdT(nothing[Id[Int => Int]]) <*> 10.pure[IdT[Maybe, *]],
      IdT(nothing[Id[Int]])
    )

    assertEquals(
      10.pure[IdT[Maybe, *]].ap(((x: Int) => x + 1).pure[IdT[Maybe, *]]).value,
      Id(11).just
    )

    assertEquals(
      10.pure[IdT[Maybe, *]].ap(IdT(nothing[Id[Int => Int]])).value,
      nothing[Id[Int]]
    )

    assertEquals(
      IdT(nothing[Id[Int]]).ap(((x: Int) => x + 1).pure[IdT[Maybe, *]]).value,
      nothing[Id[Int]]
    )

    assertEquals(
      IdT(nothing[Id[Int]]) ap ((x: Int) => x + 1).pure[IdT[Maybe, *]],
      IdT(nothing[Id[Int]])
    )

    assertEquals(
      IdT(nothing[Id[Int]]) <*> ((x: Int) => x + 1).pure[IdT[Maybe, *]],
      IdT(nothing[Id[Int]])
    )
  }*/

  /*property("IdT Functor and Applicative syntax") {
    import tech.backwards.fp.learn.Applicative.syntax._
    import tech.backwards.fp.learn.Functor.syntax._
    import tech.backwards.fp.learn.Maybe.syntax._

    assertEquals(
      IdT(Id(10).just) `<$>` (x => (y: Int) => x + y) <*> IdT(Id(1).just),
      IdT(Id(11).just)
    )

    assertEquals(
      IdT(nothing[Id[Int]]) `<$>` (x => (y: Int) => x + y) <*> IdT(Id(1).just),
      IdT(nothing[Id[Int]])
    )

    assertEquals(
      IdT(Id(10).just) `<$>` (x => (y: Int) => x + y) <*> IdT(nothing[Id[Int]]),
      IdT(nothing[Id[Int]])
    )

    val add: Int => Int => Int =
      x => y => x + y

    assertEquals(
      IdT(Id(10).just) `<$>` add <*> IdT(nothing[Id[Int]]),
      IdT(nothing[Id[Int]])
    )
  }*/

  /*property("IdT Functor and Applicative function syntax") {
    import tech.backwards.fp.learn.Applicative.syntax._
    import tech.backwards.fp.learn.Functor.syntax.function._
    import tech.backwards.fp.learn.Maybe.syntax._

    assertEquals(
      ((x: Int) => (y: Int) => x + y) `<$>` IdT(Id(10).just) <*> IdT(Id(1).just),
      IdT(Id(11).just)
    )

    assertEquals(
      ((x: Int) => (y: Int) => x + y) `<$>` IdT(nothing[Id[Int]]) <*> IdT(Id(1).just),
      IdT(nothing[Id[Int]])
    )

    assertEquals(
      ((x: Int) => (y: Int) => x + y) `<$>` IdT(Id(10).just) <*> IdT(nothing[Id[Int]]),
      IdT(nothing[Id[Int]])
    )

    val add: Int => Int => Int =
      x => y => x + y

    assertEquals(
      add `<$>` IdT(Id(10).just) <*> IdT(Id(1).just),
      IdT(Id(11).just)
    )
  }*/

  /*property("IdT for comprehension") {
    import tech.backwards.fp.learn.Functor.syntax._
    import tech.backwards.fp.learn.Maybe.syntax._
    import tech.backwards.fp.learn.Monad.syntax._

    val transformer: IdT[Maybe, Int] =
      for {
        x <- 10.pure[IdT[Maybe, *]]
        y <- 11.pure[IdT[Maybe, *]]
        z <- 12.pure[IdT[Maybe, *]]
      } yield x + y + z

    assertEquals(
      transformer.value,
      Just(Id(33))
    )

    assertEquals(
      for {
        x <- IdT.lift(10.just)
        y <- IdT.lift(11.just)
        z <- IdT.lift(12.just)
        _ <- IdT(0.just.map(Id.apply)) // Without "lift"
      } yield x + y + z,
      IdT(Id(33).just)
    )

    assertEquals(
      for {
        x <- IdT.lift(10.just)
        y <- IdT.lift(nothing[Int])
        z <- IdT.lift(12.just)
      } yield x + y + z,
      IdT(nothing[Id[Int]])
    )
  }*/
}