package tech.backwards.fp.learn

import munit.ScalaCheckSuite
import org.scalacheck.Test

class IdMonadTransformerDisjunctionSuite extends ScalaCheckSuite {
  override protected def scalaCheckTestParameters: Test.Parameters =
    super.scalaCheckTestParameters.withMinSuccessfulTests(100).withMaxDiscardRatio(10)

  property("IdT") {
    val transformer: IdT[Disjunction[String, *], Int] =
      IdT(Right(Id(10)))

    assertEquals(
      transformer.value,
      Right(Id(10))
    )
  }

  property("IdT pure") {
    val transformer: IdT[Disjunction[String, *], Int] =
      IdT.pure[Disjunction[String, *], Int](10)

    assertEquals(
      transformer.value,
      Right(Id(10))
    )
  }

  property("IdT lift") {
    val transformer: IdT[Disjunction[String, *], Int] =
      IdT.lift(Right(10))

    assertEquals(
      transformer.value,
      Right(Id(10))
    )

    assertEquals(
      IdT.lift(Right(10)).value,
      Right(Id(10))
    )

    assertEquals(
      IdT.lift(Left("whoops")).value,
      Left("whoops")
    )
  }

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

    assertEquals(
      IdT(Just(Id(10))) fmap (_ + 1),
      IdT(Just(Id(11)))
    )

    assertEquals(
      IdT(Just(Id(10))) `<$>` (_ + 1),
      IdT(Just(Id(11)))
    )

    assertEquals(
      IdT(Nothing[Id[Int]]) `<$>` (_ + 1),
      IdT(Nothing[Id[Int]])
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

    assertEquals(
      10.pure[IdT[Maybe, *]] flatMap (a => IdT(Just(Id(a + 1)))),
      IdT(Just(Id(11)))
    )

    assertEquals(
      10.pure[IdT[Maybe, *]] >>= (a => IdT(Just(Id(a + 1)))),
      IdT(Just(Id(11)))
    )

    assertEquals(
      IdT(Nothing[Id[Int]]) >>= (a => IdT(Just(Id(a + 1)))),
      IdT(Nothing[Id[Int]])
    )

    assertEquals(
      10.pure[IdT[Maybe, *]] >>= (_ => IdT(Nothing[Id[Int]])),
      IdT(Nothing[Id[Int]])
    )
  }*/

  /*property("IdT Applicative") {
    val transformerFn: IdT[Maybe, Int => Int] =
      Applicative[IdT[Maybe, *]].pure((x: Int) => x + 1)

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

    val transformerFn: IdT[Maybe, Int => Int] =
      ((x: Int) => x + 1).pure[IdT[Maybe, *]]

    val transformer: IdT[Maybe, Int] =
      10.pure[IdT[Maybe, *]]

    assertEquals(
      transformerFn.ap(transformer).value,
      Just(Id(11))
    )

    assertEquals(
      ((x: Int) => x + 1).pure[IdT[Maybe, *]].ap(10.pure[IdT[Maybe, *]]).value,
      Just(Id(11))
    )

    assertEquals(
      ((x: Int) => x + 1).pure[IdT[Maybe, *]].ap(IdT(Nothing[Id[Int]])).value,
      Nothing[Id[Int]]
    )

    assertEquals(
      IdT(Nothing[Id[Int => Int]]).ap(10.pure[IdT[Maybe, *]]).value,
      Nothing[Id[Int]]
    )

    assertEquals(
      IdT(Nothing[Id[Int => Int]]) ap 10.pure[IdT[Maybe, *]],
      IdT(Nothing[Id[Int]])
    )

    assertEquals(
      IdT(Nothing[Id[Int => Int]]) <*> 10.pure[IdT[Maybe, *]],
      IdT(Nothing[Id[Int]])
    )

    assertEquals(
      10.pure[IdT[Maybe, *]].ap(((x: Int) => x + 1).pure[IdT[Maybe, *]]).value,
      Just(Id(11))
    )

    assertEquals(
      10.pure[IdT[Maybe, *]].ap(IdT(Nothing[Id[Int => Int]])).value,
      Nothing[Id[Int]]
    )

    assertEquals(
      IdT(Nothing[Id[Int]]).ap(((x: Int) => x + 1).pure[IdT[Maybe, *]]).value,
      Nothing[Id[Int]]
    )

    assertEquals(
      IdT(Nothing[Id[Int]]) ap ((x: Int) => x + 1).pure[IdT[Maybe, *]],
      IdT(Nothing[Id[Int]])
    )

    assertEquals(
      IdT(Nothing[Id[Int]]) <*> ((x: Int) => x + 1).pure[IdT[Maybe, *]],
      IdT(Nothing[Id[Int]])
    )
  }*/

  /*property("IdT Functor and Applicative syntax") {
    import tech.backwards.fp.learn.Applicative.syntax._
    import tech.backwards.fp.learn.Functor.syntax._

    assertEquals(
      IdT(Just(Id(10))) `<$>` (x => (y: Int) => x + y) <*> IdT(Just(Id(1))),
      IdT(Just(Id(11)))
    )

    assertEquals(
      IdT(Nothing[Id[Int]]) `<$>` (x => (y: Int) => x + y) <*> IdT(Just(Id(1))),
      IdT(Nothing[Id[Int]])
    )

    assertEquals(
      IdT(Just(Id(10))) `<$>` (x => (y: Int) => x + y) <*> IdT(Nothing[Id[Int]]),
      IdT(Nothing[Id[Int]])
    )

    val add: Int => Int => Int =
      x => y => x + y

    assertEquals(
      IdT(Just(Id(10))) `<$>` add <*> IdT(Nothing[Id[Int]]),
      IdT(Nothing[Id[Int]])
    )
  }*/

  /*property("IdT Functor and Applicative function syntax") {
    import tech.backwards.fp.learn.Applicative.syntax._
    import tech.backwards.fp.learn.Functor.syntax.function._

    assertEquals(
      ((x: Int) => (y: Int) => x + y) `<$>` IdT(Just(Id(10))) <*> IdT(Just(Id(1))),
      IdT(Just(Id(11)))
    )

    assertEquals(
      ((x: Int) => (y: Int) => x + y) `<$>` IdT(Nothing[Id[Int]]) <*> IdT(Just(Id(1))),
      IdT(Nothing[Id[Int]])
    )

    assertEquals(
      ((x: Int) => (y: Int) => x + y) `<$>` IdT(Just(Id(10))) <*> IdT(Nothing[Id[Int]]),
      IdT(Nothing[Id[Int]])
    )

    val add: Int => Int => Int =
      x => y => x + y

    assertEquals(
      add `<$>` IdT(Just(Id(10))) <*> IdT(Just(Id(1))),
      IdT(Just(Id(11)))
    )
  }*/

  /*property("IdT for comprehension") {
    import tech.backwards.fp.learn.Functor.syntax._
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
        x <- IdT.lift(Just(10))
        y <- IdT.lift(Just(11))
        z <- IdT.lift(Just(12))
      } yield x + y + z,
      IdT(Just(Id(33)))
    )

    assertEquals(
      for {
        x <- IdT.lift(Just(10))
        y <- IdT.lift(Nothing[Int])
        z <- IdT.lift(Just(12))
      } yield x + y + z,
      IdT(Nothing[Id[Int]])
    )
  }*/
}