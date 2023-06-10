package tech.backwards.fp.learn

import munit.ScalaCheckSuite
import org.scalacheck.Test

/**
 * First of the following IdT suites:
 *  - IdMonadTransformerMaybeSuite
 *  - IdMonadTransformerDisjunctionSuite
 *  - IdMonadTransformerListSuite
 *  - IdMonadTransformerStateSuite
 *  - IdMonadTransformerWriterSuite
 */
class IdMonadTransformerIdSuite extends ScalaCheckSuite {
  override protected def scalaCheckTestParameters: Test.Parameters =
    super.scalaCheckTestParameters.withMinSuccessfulTests(100).withMaxDiscardRatio(10)

  property("IdT") {
    val transformer: IdT[Id, Int] =
      IdT(Id(Id(10)))

    assertEquals(
      transformer.value,
      Id(Id(10))
    )
  }

  property("IdT pure") {
    val transformer: IdT[Id, Int] =
      IdT.pure[Id, Int](10)

    assertEquals(
      transformer.value,
      Id(Id(10))
    )
  }

  property("IdT lift") {
    val transformer: IdT[Id, Int] =
      IdT.lift(Id(10))

    assertEquals(
      transformer.value,
      Id(Id(10))
    )

    assertEquals(
      IdT.lift(Id(10)).value,
      Id(Id(10))
    )
  }

  property("IdT Functor") {
    val transformer: IdT[Id, Int] =
      IdT(Id(Id(10)))

    assertEquals(
      Functor[IdT[Id, *]].fmap(transformer)(_ + 1).value,
      Id(Id(11))
    )

    assertEquals(
      Functor[IdT[Id, *]].fmap(IdT(Id(Id(10))))(_ + 1).value,
      Id(Id(11))
    )
  }

  property("IdT Functor syntax") {
    import tech.backwards.fp.learn.Functor.syntax._

    val transformer: IdT[Id, Int] =
      IdT(Id(Id(10)))

    assertEquals(
      transformer.fmap(_ + 1).value,
      Id(Id(11))
    )

    assertEquals(
      IdT(Id(Id(10))).fmap(_ + 1).value,
      Id(Id(11))
    )

    assertEquals(
      IdT(Id(Id(10))) fmap (_ + 1),
      IdT(Id(Id(11)))
    )

    assertEquals(
      IdT(Id(Id(10))) `<$>` (_ + 1),
      IdT(Id(Id(11)))
    )
  }

  property("IdT Monad") {
    val transformer: IdT[Id, Int] =
      Monad[IdT[Id, *]].pure(10)

    assertEquals(
      Monad[IdT[Id, *]].flatMap(transformer)(a => Monad[IdT[Id, *]].pure(a + 1)).value,
      Id(Id(11))
    )

    assertEquals(
      Monad[IdT[Id, *]].flatMap(Monad[IdT[Id, *]].pure(10))(a => Monad[IdT[Id, *]].pure(a + 1)).value,
      Id(Id(11))
    )
  }

  property("IdT Monad syntax") {
    import tech.backwards.fp.learn.Monad.syntax._

    val transformer: IdT[Id, Int] =
      10.pure[IdT[Id, *]]

    assertEquals(
      transformer.flatMap(a => (a + 1).pure[IdT[Id, *]]).value,
      Id(Id(11))
    )

    assertEquals(
      10.pure[IdT[Id, *]].flatMap(a => (a + 1).pure[IdT[Id, *]]).value,
      Id(Id(11))
    )

    assertEquals(
      10.pure[IdT[Id, *]] flatMap (a => IdT(Id(Id(a + 1)))),
      IdT(Id(Id(11)))
    )

    assertEquals(
      10.pure[IdT[Id, *]] >>= (a => IdT(Id(Id(a + 1)))),
      IdT(Id(Id(11)))
    )
  }

  property("IdT Applicative") {
    val transformerFn: IdT[Id, Int => Int] =
      Applicative[IdT[Id, *]].pure(_ + 1)

    val transformer: IdT[Id, Int] =
      Applicative[IdT[Id, *]].pure(10)

    assertEquals(
      Applicative[IdT[Id, *]].ap(transformerFn)(transformer).value,
      Id(Id(11))
    )

    assertEquals(
      Applicative[IdT[Id, *]].ap(Applicative[IdT[Id, *]].pure((x: Int) => x + 1))(Applicative[IdT[Id, *]].pure(10)).value,
      Id(Id(11))
    )
  }

  property("IdT Applicative syntax") {
    import tech.backwards.fp.learn.Applicative.syntax._

    val transformerFn: IdT[Id, Int => Int] =
      ((x: Int) => x + 1).pure[IdT[Id, *]]

    val transformer: IdT[Id, Int] =
      10.pure[IdT[Id, *]]

    assertEquals(
      transformerFn.ap(transformer).value,
      Id(Id(11))
    )

    assertEquals(
      ((x: Int) => x + 1).pure[IdT[Id, *]].ap(10.pure[IdT[Id, *]]).value,
      Id(Id(11))
    )

    assertEquals(
      10.pure[IdT[Id, *]].ap(((x: Int) => x + 1).pure[IdT[Id, *]]).value,
      Id(Id(11))
    )
  }

  property("IdT Functor and Applicative syntax") {
    import tech.backwards.fp.learn.Applicative.syntax._
    import tech.backwards.fp.learn.Functor.syntax._

    assertEquals(
      IdT(Id(Id(10))) `<$>` (x => (y: Int) => x + y) <*> IdT(Id(Id(1))),
      IdT(Id(Id(11)))
    )

    val add: Int => Int => Int =
      x => y => x + y

    assertEquals(
      IdT(Id(Id(10))) `<$>` add <*> IdT(Id(Id(1))),
      IdT(Id(Id(11)))
    )
  }

  property("IdT Functor and Applicative function syntax") {
    import tech.backwards.fp.learn.Applicative.syntax._
    import tech.backwards.fp.learn.Functor.syntax.function._

    assertEquals(
      ((x: Int) => (y: Int) => x + y) `<$>` IdT(Id(Id(10))) <*> IdT(Id(Id(1))),
      IdT(Id(Id(11)))
    )

    val add: Int => Int => Int =
      x => y => x + y

    assertEquals(
      add `<$>` IdT(Id(Id(10))) <*> IdT(Id(Id(1))),
      IdT(Id(Id(11)))
    )
  }

  property("IdT for comprehension") {
    import tech.backwards.fp.learn.Functor.syntax._
    import tech.backwards.fp.learn.Monad.syntax._

    val transformer: IdT[Id, Int] =
      for {
        x <- 10.pure[IdT[Id, *]]
        y <- 11.pure[IdT[Id, *]]
        z <- 12.pure[IdT[Id, *]]
      } yield x + y + z

    assertEquals(
      transformer.value,
      Id(Id(33))
    )

    assertEquals(
      for {
        x <- IdT.lift(Id(10))
        y <- IdT.lift(Id(11))
        z <- IdT.lift(Id(12))
        _ <- IdT(Id(0).map(Id.apply)) // Without "lift"
      } yield x + y + z,
      IdT(Id(Id(33)))
    )
  }
}