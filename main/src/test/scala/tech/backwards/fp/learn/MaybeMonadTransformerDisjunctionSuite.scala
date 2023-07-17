package tech.backwards.fp.learn

import munit.ScalaCheckSuite
import org.scalacheck.Test

class MaybeMonadTransformerDisjunctionSuite extends ScalaCheckSuite {
  override protected def scalaCheckTestParameters: Test.Parameters =
    super.scalaCheckTestParameters.withMinSuccessfulTests(100).withMaxDiscardRatio(10)

  property("MaybeT") {
    val transformer: MaybeT[Disjunction[String, *], Int] =
      MaybeT(Right(Just(10)))

    assertEquals(
      transformer.value,
      Right(Just(10))
    )

    assertEquals(
      MaybeT(Right(Nothing[Int])).value,
      Right(Nothing[Int])
    )

    assertEquals(
      MaybeT(Left[String, Maybe[Int]]("whoops")).value,
      Left[String, Maybe[Int]]("whoops")
    )
  }

  property("MaybeT syntax") {
    import tech.backwards.fp.learn.Disjunction.syntax._

    assertEquals(
      MaybeT(Nothing[Int].right).value,
      Nothing[Int].right
    )

    assertEquals(
      MaybeT("whoops".left[Maybe[Int]]).value,
      "whoops".left[Maybe[Int]]
    )
  }

  property("MaybeT pure") {
    val transformer: MaybeT[Disjunction[String, *], Int] =
      MaybeT.pure[Disjunction[String, *], Int](10)

    assertEquals(
      transformer.value,
      Right(Just(10))
    )
  }

  property("MaybeT lift") {
    val transformer: MaybeT[Disjunction[String, *], Int] =
      MaybeT.lift(Right(10))

    assertEquals(
      transformer.value,
      Right(Just(10))
    )

    assertEquals(
      MaybeT.lift(Right(10)).value,
      Right(Just(10))
    )

    assertEquals(
      MaybeT.lift(Left("whoops")).value,
      Left("whoops")
    )
  }

  property("MaybeT Functor") {
    val transformer: MaybeT[Disjunction[String, *], Int] =
      MaybeT(Right(Just(10)))

    assertEquals(
      Functor[MaybeT[Disjunction[String, *], *]].fmap(transformer)(_ + 1).value,
      Right(Just(11))
    )

    assertEquals(
      Functor[MaybeT[Disjunction[String, *], *]].fmap(MaybeT(Right(Just(10))))(_ + 1).value,
      Right(Just(11))
    )

    assertEquals(
      Functor[MaybeT[Disjunction[String, *], *]].fmap(MaybeT(Right(Nothing[Int])))(_ + 1).value,
      Right(Nothing[Int])
    )

    assertEquals(
      Functor[MaybeT[Disjunction[String, *], *]].fmap(MaybeT(Left[String, Maybe[Int]]("whoops")))(_ + 1).value,
      Left("whoops")
    )
  }

  property("MaybeT Functor syntax") {
    import tech.backwards.fp.learn.Disjunction.syntax._
    import tech.backwards.fp.learn.Functor.syntax._
    import tech.backwards.fp.learn.Maybe.syntax._

    val transformer: MaybeT[Disjunction[String, *], Int] =
      MaybeT(10.just.right)

    assertEquals(
      transformer.fmap(_ + 1).value,
      11.just.right
    )

    assertEquals(
      MaybeT(10.just.right).fmap(_ + 1).value,
      11.just.right
    )

    assertEquals(
      MaybeT(10.just.right) fmap (_ + 1),
      MaybeT(11.just.right)
    )

    assertEquals(
      MaybeT(10.just.right) `<$>` (_ + 1),
      MaybeT(11.just.right)
    )

    assertEquals(
      MaybeT(nothing[Int].right) `<$>` (_ + 1),
      MaybeT(nothing[Int].right)
    )

    assertEquals(
      MaybeT("whoops".left[Maybe[Int]]) `<$>` (_ + 1),
      MaybeT("whoops".left[Maybe[Int]])
    )
  }

  property("MaybeT Monad") {
    val transformer: MaybeT[Disjunction[String, *], Int] =
      Monad[MaybeT[Disjunction[String, *], *]].pure(10)

    assertEquals(
      Monad[MaybeT[Disjunction[String, *], *]].flatMap(transformer)(a => Monad[MaybeT[Disjunction[String, *], *]].pure(a + 1)).value,
      Right(Just(11))
    )

    assertEquals(
      Monad[MaybeT[Disjunction[String, *], *]].flatMap(Monad[MaybeT[Disjunction[String, *], *]].pure(10))(a => Monad[MaybeT[Disjunction[String, *], *]].pure(a + 1)).value,
      Right(Just(11))
    )

    assertEquals(
      Monad[MaybeT[Disjunction[String, *], *]].flatMap(MaybeT(Right(Nothing[Int])))(a => Monad[MaybeT[Disjunction[String, *], *]].pure(a + 1)).value,
      Right(Nothing[Int])
    )

    assertEquals(
      Monad[MaybeT[Disjunction[String, *], *]].flatMap(Monad[MaybeT[Disjunction[String, *], *]].pure(10))(_ => MaybeT(Right(Nothing[Int]))).value,
      Right(Nothing[Int])
    )

    assertEquals(
      Monad[MaybeT[Disjunction[String, *], *]].flatMap(MaybeT(Right(Nothing[Int])))(_ => MaybeT(Right(Nothing[Int]))).value,
      Right(Nothing[Int])
    )

    assertEquals(
      Monad[MaybeT[Disjunction[String, *], *]].flatMap(MaybeT(Left[String, Maybe[Int]]("whoops")))(_ => MaybeT(Right(Nothing[Int]))).value,
      Left("whoops")
    )

    assertEquals(
      Monad[MaybeT[Disjunction[String, *], *]].flatMap(MaybeT(Right(Nothing[Int])))(_ => MaybeT(Left[String, Maybe[Int]]("whoops"))).value,
      Right(Nothing[Int])
    )

    assertEquals(
      Monad[MaybeT[Disjunction[String, *], *]].flatMap(Monad[MaybeT[Disjunction[String, *], *]].pure(10))(_ => MaybeT(Left[String, Maybe[Int]]("whoops"))).value,
      Left("whoops")
    )
  }

  property("MaybeT Monad syntax") {
    import tech.backwards.fp.learn.Disjunction.syntax._
    import tech.backwards.fp.learn.Maybe.syntax._
    import tech.backwards.fp.learn.Monad.syntax._

    val transformer: MaybeT[Disjunction[String, *], Int] =
      10.pure[MaybeT[Disjunction[String, *], *]]

    assertEquals(
      transformer.flatMap(a => (a + 1).pure[MaybeT[Disjunction[String, *], *]]).value,
      11.just.right
    )

    assertEquals(
      10.pure[MaybeT[Disjunction[String, *], *]].flatMap(a => (a + 1).pure[MaybeT[Disjunction[String, *], *]]).value,
      11.just.right
    )

    assertEquals(
      MaybeT(nothing[Int].right[String]).flatMap(a => (a + 1).pure[MaybeT[Disjunction[String, *], *]]).value,
      nothing[Int].right
    )

    assertEquals(
      10.pure[MaybeT[Disjunction[String, *], *]].flatMap(_ => MaybeT(nothing[Int].right)).value,
      nothing[Int].right
    )

    assertEquals(
      MaybeT(nothing[Int].right).flatMap(_ => MaybeT(Nothing[Int].right)).value,
      nothing[Int].right
    )

    assertEquals(
      MaybeT("whoops".left[Maybe[Int]]).flatMap(_ => MaybeT(nothing[Int].right)).value,
      "whoops".left
    )

    assertEquals(
      MaybeT(nothing[Int].right[String]).flatMap(_ => MaybeT("whoops".left[Maybe[Int]])).value,
      nothing[Int].right
    )

    assertEquals(
      MaybeT(10.just.right[String]).flatMap(_ => MaybeT("whoops".left[Maybe[Int]])).value,
      "whoops".left[Maybe[Int]]
    )
  }

  /*property("MaybeT Applicative") {
    val transformerFn: MaybeT[Maybe, Int => Int] =
      Applicative[MaybeT[Maybe, *]].pure(_ + 1)

    val transformer: MaybeT[Maybe, Int] =
      Applicative[MaybeT[Maybe, *]].pure(10)

    assertEquals(
      Applicative[MaybeT[Maybe, *]].ap(transformerFn)(transformer).value,
      Just(Just(11))
    )

    assertEquals(
      Applicative[MaybeT[Maybe, *]].ap(Applicative[MaybeT[Maybe, *]].pure((x: Int) => x + 1))(Applicative[MaybeT[Maybe, *]].pure(10)).value,
      Just(Just(11))
    )

    assertEquals(
      Applicative[MaybeT[Maybe, *]].ap(MaybeT(Just(Nothing[Int => Int])))(Applicative[MaybeT[Maybe, *]].pure(10)).value,
      Just(Nothing[Int])
    )

    assertEquals(
      Applicative[MaybeT[Maybe, *]].ap(MaybeT(Nothing[Maybe[Int => Int]]))(Applicative[MaybeT[Maybe, *]].pure(10)).value,
      Nothing[Maybe[Int]]
    )

    assertEquals(
      Applicative[MaybeT[Maybe, *]].ap(Applicative[MaybeT[Maybe, *]].pure((x: Int) => x + 1))(MaybeT(Just(Nothing[Int]))).value,
      Just(Nothing[Int])
    )

    assertEquals(
      Applicative[MaybeT[Maybe, *]].ap(Applicative[MaybeT[Maybe, *]].pure((x: Int) => x + 1))(MaybeT(Nothing[Maybe[Int]])).value,
      Nothing[Maybe[Int]]
    )
  }*/

  /*property("MaybeT Applicative syntax") {
    import tech.backwards.fp.learn.Applicative.syntax._

    val transformerFn: MaybeT[Maybe, Int => Int] =
      ((x: Int) => x + 1).pure[MaybeT[Maybe, *]]

    val transformer: MaybeT[Maybe, Int] =
      10.pure[MaybeT[Maybe, *]]

    assertEquals(
      transformerFn.ap(transformer).value,
      Just(Just(11))
    )

    assertEquals(
      ((x: Int) => x + 1).pure[MaybeT[Maybe, *]].ap(10.pure[MaybeT[Maybe, *]]).value,
      Just(Just(11))
    )

    assertEquals(
      MaybeT(Just(Nothing[Int => Int])).ap(10.pure[MaybeT[Maybe, *]]).value,
      Just(Nothing[Int])
    )

    assertEquals(
      MaybeT(Nothing[Maybe[Int => Int]]).ap(10.pure[MaybeT[Maybe, *]]).value,
      Nothing[Maybe[Int]]
    )

    assertEquals(
      ((x: Int) => x + 1).pure[MaybeT[Maybe, *]].ap(MaybeT(Just(Nothing[Int]))).value,
      Just(Nothing[Int])
    )

    assertEquals(
      ((x: Int) => x + 1).pure[MaybeT[Maybe, *]].ap(MaybeT(Nothing[Maybe[Int]])).value,
      Nothing[Maybe[Int]]
    )
  }*/

  /*property("MaybeT Functor and Applicative syntax") {
    import tech.backwards.fp.learn.Applicative.syntax._
    import tech.backwards.fp.learn.Functor.syntax._
    import tech.backwards.fp.learn.Maybe.syntax._

    assertEquals(
      MaybeT(10.just.just) `<$>` (x => (y: Int) => x + y) <*> MaybeT(1.just.just),
      MaybeT(11.just.just)
    )

    assertEquals(
      MaybeT(Just(nothing[Int])) `<$>` (x => (y: Int) => x + y) <*> MaybeT(1.just.just),
      MaybeT(Just(nothing[Int]))
    )

    assertEquals(
      MaybeT(Nothing[Maybe[Int]]) `<$>` (x => (y: Int) => x + y) <*> MaybeT(1.just.just),
      MaybeT(Nothing[Maybe[Int]])
    )

    assertEquals(
      MaybeT(10.just.just) `<$>` (x => (y: Int) => x + y) <*> MaybeT(Just(nothing[Int])),
      MaybeT(Just(nothing[Int]))
    )

    assertEquals(
      MaybeT(10.just.just) `<$>` (x => (y: Int) => x + y) <*> MaybeT(Nothing[Maybe[Int]]),
      MaybeT(Nothing[Maybe[Int]])
    )

    val add: Int => Int => Int =
      x => y => x + y

    assertEquals(
      MaybeT(10.just.just) `<$>` add <*> MaybeT(Just(nothing[Int])),
      MaybeT(Just(nothing[Int]))
    )
  }*/

  /*property("MaybeT Functor and Applicative function syntax") {
    import tech.backwards.fp.learn.Applicative.syntax._
    import tech.backwards.fp.learn.Functor.syntax.function._
    import tech.backwards.fp.learn.Maybe.syntax._

    assertEquals(
      ((x: Int) => (y: Int) => x + y) `<$>` MaybeT(10.just.just) <*> MaybeT(1.just.just),
      MaybeT(11.just.just)
    )

    assertEquals(
      ((x: Int) => (y: Int) => x + y) `<$>` MaybeT(Just(nothing[Int])) <*> MaybeT(1.just.just),
      MaybeT(Just(nothing[Int]))
    )

    assertEquals(
      ((x: Int) => (y: Int) => x + y) `<$>` MaybeT(Nothing[Maybe[Int]]) <*> MaybeT(1.just.just),
      MaybeT(Nothing[Maybe[Int]])
    )

    assertEquals(
      ((x: Int) => (y: Int) => x + y) `<$>` MaybeT(10.just.just) <*> MaybeT(Just(nothing[Int])),
      MaybeT(Just(nothing[Int]))
    )

    assertEquals(
      ((x: Int) => (y: Int) => x + y) `<$>` MaybeT(10.just.just) <*> MaybeT(Nothing[Maybe[Int]]),
      MaybeT(Nothing[Maybe[Int]])
    )

    val add: Int => Int => Int =
      x => y => x + y

    assertEquals(
      add `<$>` MaybeT(10.just.just) <*> MaybeT(1.just.just),
      MaybeT(11.just.just)
    )
  }*/

  /*property("MaybeT for comprehension") {
    import tech.backwards.fp.learn.Functor.syntax._
    import tech.backwards.fp.learn.Maybe.syntax._
    import tech.backwards.fp.learn.Monad.syntax._

    val transformer: MaybeT[Maybe, Int] =
      for {
        x <- 10.pure[MaybeT[Maybe, *]]
        y <- 11.pure[MaybeT[Maybe, *]]
        z <- 12.pure[MaybeT[Maybe, *]]
      } yield x + y + z

    assertEquals(
      transformer.value,
      Just(Just(33))
    )

    assertEquals(
      for {
        x <- MaybeT.lift(Just(10))
        y <- MaybeT.lift(Just(11))
        z <- MaybeT.lift(Just(12))
        _ <- MaybeT(Just(0).map(Just.apply)) // Without "lift"
      } yield x + y + z,
      MaybeT(Just(Just(33)))
    )

    assertEquals(
      for {
        x <- MaybeT.lift(Just(10))
        y <- MaybeT(Just(nothing[Int]))
        z <- MaybeT.lift(Just(12))
      } yield x + y + z,
      MaybeT(Just(nothing[Int]))
    )

    assertEquals(
      for {
        x <- MaybeT.lift(Just(10))
        y <- MaybeT(Nothing[Maybe[Int]])
        z <- MaybeT.lift(Just(12))
      } yield x + y + z,
      MaybeT(Nothing[Maybe[Int]])
    )
  }*/
}