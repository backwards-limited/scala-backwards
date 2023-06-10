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

  /*property("IdT pure") {
    val transformer: IdT[Maybe, Int] =
      IdT.pure[Maybe, Int](10)

    assertEquals(
      transformer.value,
      Just(Id(10))
    )
  }*/

  /*property("IdT lift") {
    val transformer: IdT[Maybe, Int] =
      IdT.lift(Just(10))

    assertEquals(
      transformer.value,
      Just(Id(10))
    )

    assertEquals(
      IdT.lift(Just(10)).value,
      Just(Id(10))
    )

    assertEquals(
      IdT.lift(Nothing[Int]).value,
      Nothing[Id[Int]]
    )
  }*/

  /*property("IdT Functor") {
    val transformer: IdT[Maybe, Int] =
      IdT(Just(Id(10)))

    assertEquals(
      Functor[IdT[Maybe, *]].fmap(transformer)(_ + 1).value,
      Just(Id(11))
    )

    assertEquals(
      Functor[IdT[Maybe, *]].fmap(IdT(Just(Id(10))))(_ + 1).value,
      Just(Id(11))
    )

    assertEquals(
      Functor[IdT[Maybe, *]].fmap(IdT(Nothing[Id[Int]]))(_ + 1).value,
      Nothing[Id[Int]]
    )
  }*/

  /*property("IdT Functor syntax") {
    import tech.backwards.fp.learn.Functor.syntax._
    import tech.backwards.fp.learn.Maybe.syntax._

    val transformer: IdT[Maybe, Int] =
      IdT(Id(10).just)

    assertEquals(
      transformer.fmap(_ + 1).value,
      Id(11).just
    )

    assertEquals(
      IdT(Id(10).just).fmap(_ + 1).value,
      Id(11).just
    )

    assertEquals(
      IdT(Id(10).just) fmap (_ + 1),
      IdT(Id(11).just)
    )

    assertEquals(
      IdT(Id(10).just) `<$>` (_ + 1),
      IdT(Id(11).just)
    )

    assertEquals(
      IdT(nothing[Id[Int]]) `<$>` (_ + 1),
      IdT(nothing[Id[Int]])
    )
  }*/

  /*property("IdT Monad") {
    val transformer: IdT[Maybe, Int] =
      Monad[IdT[Maybe, *]].pure(10)

    assertEquals(
      Monad[IdT[Maybe, *]].flatMap(transformer)(a => Monad[IdT[Maybe, *]].pure(a + 1)).value,
      Just(Id(11))
    )

    assertEquals(
      Monad[IdT[Maybe, *]].flatMap(Monad[IdT[Maybe, *]].pure(10))(a => Monad[IdT[Maybe, *]].pure(a + 1)).value,
      Just(Id(11))
    )

    assertEquals(
      Monad[IdT[Maybe, *]].flatMap(IdT(Nothing[Id[Int]]))(a => Monad[IdT[Maybe, *]].pure(a + 1)).value,
      Nothing[Id[Int]]
    )

    assertEquals(
      Monad[IdT[Maybe, *]].flatMap(Monad[IdT[Maybe, *]].pure(10))(_ => IdT(Nothing[Id[Int]])).value,
      Nothing[Id[Int]]
    )
  }*/

  /*property("IdT Monad syntax") {
    import tech.backwards.fp.learn.Maybe.syntax._
    import tech.backwards.fp.learn.Monad.syntax._

    val transformer: IdT[Maybe, Int] =
      10.pure[IdT[Maybe, *]]

    assertEquals(
      transformer.flatMap(a => (a + 1).pure[IdT[Maybe, *]]).value,
      Id(11).just
    )

    assertEquals(
      10.pure[IdT[Maybe, *]].flatMap(a => (a + 1).pure[IdT[Maybe, *]]).value,
      Id(11).just
    )

    assertEquals(
      10.pure[IdT[Maybe, *]] flatMap (a => IdT(Id(a + 1).just)),
      IdT(Id(11).just)
    )

    assertEquals(
      10.pure[IdT[Maybe, *]] >>= (a => IdT(Id(a + 1).just)),
      IdT(Id(11).just)
    )

    assertEquals(
      IdT(nothing[Id[Int]]) >>= (a => IdT(Id(a + 1).just)),
      IdT(nothing[Id[Int]])
    )

    assertEquals(
      10.pure[IdT[Maybe, *]] >>= (_ => IdT(nothing[Id[Int]])),
      IdT(nothing[Id[Int]])
    )
  }*/

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