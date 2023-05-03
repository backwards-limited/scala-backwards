package tech.backwards.fp.learn

import munit.ScalaCheckSuite
import org.scalacheck.Test

class IdMonadTransformerSuite extends ScalaCheckSuite {
  override protected def scalaCheckTestParameters: Test.Parameters =
    super.scalaCheckTestParameters.withMinSuccessfulTests(100).withMaxDiscardRatio(10)

  property("IdT") {
    val transformer: IdT[Maybe, Int] =
      IdT(Just(Id(10)))

    assertEquals(transformer.value, Just(Id(10)))
  }

  property("IdT pure") {
    val transformer: IdT[Maybe, Int] =
      IdT.pure[Maybe, Int](10)

    assertEquals(transformer.value, Just(Id(10)))
  }

  property("IdT Functor") {
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
  }

  property("IdT Functor syntax") {
    import tech.backwards.fp.learn.Functor.syntax._

    val transformer: IdT[Maybe, Int] =
      IdT(Just(Id(10)))

    assertEquals(
      transformer.fmap(_ + 1).value,
      Just(Id(11))
    )

    assertEquals(
      IdT(Just(Id(10))).fmap(_ + 1).value,
      Just(Id(11))
    )
  }

  property("IdT Monad") {
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
  }

  property("IdT Monad syntax") {
    import tech.backwards.fp.learn.Monad.syntax._

    val transformer: IdT[Maybe, Int] =
      10.pure[IdT[Maybe, *]]

    assertEquals(
      transformer.flatMap(a => (a + 1).pure[IdT[Maybe, *]]).value,
      Just(Id(11))
    )

    assertEquals(
      10.pure[IdT[Maybe, *]].flatMap(a => (a + 1).pure[IdT[Maybe, *]]).value,
      Just(Id(11))
    )
  }

  /*property("IdT Applicative") {
    import tech.backwards.fp.learn.Applicative.syntax._

    val transformer: IdT[Maybe, Int] =
      10.pure[IdT[Maybe, *]]

    assertEquals(transformer.value, Just(Id(10)))
  }*/

  /*property("IdT Applicative syntax") {
    import tech.backwards.fp.learn.Applicative.syntax._

    val transformer: IdT[Maybe, Int] =
      10.pure[IdT[Maybe, *]]

    assertEquals(transformer.value, Just(Id(10)))
  }*/
}