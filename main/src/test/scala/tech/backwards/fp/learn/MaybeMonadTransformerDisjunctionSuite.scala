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

  property("MaybeT Applicative") {
    val transformerFn: MaybeT[Disjunction[String, *], Int => Int] =
      Applicative[MaybeT[Disjunction[String, *], *]].pure(_ + 1)

    val transformer: MaybeT[Disjunction[String, *], Int] =
      Applicative[MaybeT[Disjunction[String, *], *]].pure(10)

    assertEquals(
      Applicative[MaybeT[Disjunction[String, *], *]].ap(transformerFn)(transformer).value,
      Right(Just(11))
    )

    assertEquals(
      Applicative[MaybeT[Disjunction[String, *], *]].ap(Applicative[MaybeT[Disjunction[String, *], *]].pure((x: Int) => x + 1))(Applicative[MaybeT[Disjunction[String, *], *]].pure(10)).value,
      Right(Just(11))
    )

    assertEquals(
      Applicative[MaybeT[Disjunction[String, *], *]].ap(MaybeT(Right(Nothing[Int => Int])))(Applicative[MaybeT[Disjunction[String, *], *]].pure(10)).value,
      Right(Nothing[Int])
    )

    assertEquals(
      Applicative[MaybeT[Disjunction[String, *], *]].ap(MaybeT(Left[String, Maybe[Int => Int]]("whoops")))(Applicative[MaybeT[Disjunction[String, *], *]].pure(10)).value,
      Left("whoops")
    )

    assertEquals(
      Applicative[MaybeT[Disjunction[String, *], *]].ap(Applicative[MaybeT[Disjunction[String, *], *]].pure((x: Int) => x + 1))(MaybeT(Right(Nothing[Int]))).value,
      Right(Nothing[Int])
    )

    assertEquals(
      Applicative[MaybeT[Disjunction[String, *], *]].ap(Applicative[MaybeT[Disjunction[String, *], *]].pure((x: Int) => x + 1))(MaybeT(Left[String, Maybe[Int]]("whoops"))).value,
      Left("whoops")
    )
  }

  property("MaybeT Applicative syntax") {
    import tech.backwards.fp.learn.Applicative.syntax._

    val transformerFn: MaybeT[Disjunction[String, *], Int => Int] =
      ((x: Int) => x + 1).pure[MaybeT[Disjunction[String, *], *]]

    val transformer: MaybeT[Disjunction[String, *], Int] =
      10.pure[MaybeT[Disjunction[String, *], *]]

    assertEquals(
      transformerFn.ap(transformer).value,
      Right(Just(11))
    )

    assertEquals(
      ((x: Int) => x + 1).pure[MaybeT[Disjunction[String, *], *]].ap(10.pure[MaybeT[Disjunction[String, *], *]]).value,
      Right(Just(11))
    )

    assertEquals(
      MaybeT(Right[String, Maybe[Int => Int]](Nothing[Int => Int])).ap(10.pure[MaybeT[Disjunction[String, *], *]]).value,
      Right(Nothing[Int])
    )

    assertEquals(
      MaybeT(Left[String, Maybe[Int => Int]]("whoops")).ap(10.pure[MaybeT[Disjunction[String, *], *]]).value,
      Left("whoops")
    )

    assertEquals(
      ((x: Int) => x + 1).pure[MaybeT[Disjunction[String, *], *]].ap(MaybeT(Right(Nothing[Int]))).value,
      Right(Nothing[Int])
    )

    assertEquals(
      ((x: Int) => x + 1).pure[MaybeT[Disjunction[String, *], *]].ap(MaybeT(Left[String, Maybe[Int]]("whoops"))).value,
      Left("whoops")
    )
  }

  property("MaybeT Functor and Applicative syntax") {
    import tech.backwards.fp.learn.Applicative.syntax._
    import tech.backwards.fp.learn.Disjunction.syntax._
    import tech.backwards.fp.learn.Functor.syntax._
    import tech.backwards.fp.learn.Maybe.syntax._

    assertEquals(
      MaybeT(10.just.right) `<$>` (x => (y: Int) => x + y) <*> MaybeT(1.just.right),
      MaybeT(11.just.right)
    )

    assertEquals(
      MaybeT(Right(nothing[Int])) `<$>` (x => (y: Int) => x + y) <*> MaybeT(1.just.right),
      MaybeT(Right(nothing[Int]))
    )

    assertEquals(
      MaybeT("whoops".left[Maybe[Int]]) `<$>` (x => (y: Int) => x + y) <*> MaybeT(1.just.right),
      MaybeT("whoops".left[Maybe[Int]])
    )

    assertEquals(
      MaybeT(10.just.right) `<$>` (x => (y: Int) => x + y) <*> MaybeT(Right(nothing[Int])),
      MaybeT(Right(nothing[Int]))
    )

    assertEquals(
      MaybeT(10.just.right[String]) `<$>` (x => (y: Int) => x + y) <*> MaybeT("whoops".left[Maybe[Int]]),
      MaybeT("whoops".left[Maybe[Int]])
    )

    val add: Int => Int => Int =
      x => y => x + y

    assertEquals(
      MaybeT(10.just.right) `<$>` add <*> MaybeT(Right(nothing[Int])),
      MaybeT(Right(nothing[Int]))
    )
  }

  property("MaybeT Functor and Applicative function syntax") {
    import tech.backwards.fp.learn.Applicative.syntax._
    import tech.backwards.fp.learn.Disjunction.syntax._
    import tech.backwards.fp.learn.Functor.syntax.function._
    import tech.backwards.fp.learn.Maybe.syntax._

    assertEquals(
      ((x: Int) => (y: Int) => x + y) `<$>` MaybeT(10.just.right) <*> MaybeT(1.just.right),
      MaybeT(11.just.right)
    )

    assertEquals(
      ((x: Int) => (y: Int) => x + y) `<$>` MaybeT(nothing[Int].right) <*> MaybeT(1.just.right),
      MaybeT(nothing[Int].right)
    )

    assertEquals(
      ((x: Int) => (y: Int) => x + y) `<$>` MaybeT("whoops".left[Maybe[Int]]) <*> MaybeT(1.just.right),
      MaybeT("whoops".left[Maybe[Int]])
    )

    assertEquals(
      ((x: Int) => (y: Int) => x + y) `<$>` MaybeT(10.just.right) <*> MaybeT(nothing[Int].right),
      MaybeT(nothing[Int].right)
    )

    assertEquals(
      ((x: Int) => (y: Int) => x + y) `<$>` MaybeT(10.just.right[String]) <*> MaybeT("whoops".left[Maybe[Int]]),
      MaybeT("whoops".left[Maybe[Int]])
    )

    val add: Int => Int => Int =
      x => y => x + y

    assertEquals(
      add `<$>` MaybeT(10.just.right) <*> MaybeT(1.just.right),
      MaybeT(11.just.right)
    )
  }

  property("MaybeT for comprehension") {
    import tech.backwards.fp.learn.Disjunction.syntax._
    import tech.backwards.fp.learn.Functor.syntax._
    import tech.backwards.fp.learn.Maybe.syntax._
    import tech.backwards.fp.learn.Monad.syntax._

    val transformer: MaybeT[Disjunction[String, *], Int] =
      for {
        x <- 10.pure[MaybeT[Disjunction[String, *], *]]
        y <- 11.pure[MaybeT[Disjunction[String, *], *]]
        z <- 12.pure[MaybeT[Disjunction[String, *], *]]
      } yield x + y + z

    assertEquals(
      transformer.value,
      Right(Just(33))
    )

    assertEquals(
      for {
        x <- MaybeT.lift(Right(10))
        y <- MaybeT.lift(Right(11))
        z <- MaybeT.lift(Right(12))
        _ <- MaybeT(Right(0).map(Just.apply)) // Without "lift"
      } yield x + y + z,
      MaybeT(Right(Just(33)))
    )

    assertEquals(
      for {
        x <- MaybeT.lift(10.right[String])
        y <- MaybeT(nothing[Int].right[String])
        z <- MaybeT.lift(12.right[String])
      } yield x + y + z,
      MaybeT(nothing[Int].right[String])
    )

    assertEquals(
      for {
        x <- MaybeT.lift(10.right[String])
        y <- MaybeT("whoops".left[Maybe[Int]])
        z <- MaybeT.lift(12.right[String])
      } yield x + y + z,
      MaybeT("whoops".left[Maybe[Int]])
    )
  }
}