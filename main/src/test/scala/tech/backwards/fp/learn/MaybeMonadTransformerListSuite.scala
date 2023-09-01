package tech.backwards.fp.learn

import munit.ScalaCheckSuite
import org.scalacheck.Test

class MaybeMonadTransformerListSuite extends ScalaCheckSuite {
  override protected def scalaCheckTestParameters: Test.Parameters =
    super.scalaCheckTestParameters.withMinSuccessfulTests(100).withMaxDiscardRatio(10)

  property("MaybeT") {
    val transformer: MaybeT[List, Int] =
      MaybeT(List(Just(10)))

    assertEquals(
      transformer.value,
      List(Just(10))
    )

    assertEquals(
      MaybeT(List(Nothing[Int])).value,
      List(Nothing[Int])
    )

    assertEquals(
      MaybeT(List.empty[Maybe[Int]]).value,
      Nil
    )
  }

  property("MaybeT pure") {
    val transformer: MaybeT[List, Int] =
      MaybeT.pure[List, Int](10)

    assertEquals(
      transformer.value,
      List(Just(10))
    )
  }

  property("MaybeT lift") {
    val transformer: MaybeT[List, Int] =
      MaybeT.lift(List(10))

    assertEquals(
      transformer.value,
      List(Just(10))
    )

    assertEquals(
      MaybeT.lift(List(10)).value,
      List(Just(10))
    )

    assertEquals(
      MaybeT.lift(List.empty[Maybe[Int]]).value,
      Nil
    )
  }

  property("MaybeT Functor") {
    val transformer: MaybeT[List, Int] =
      MaybeT(List(Just(10)))

    assertEquals(
      Functor[MaybeT[List, *]].fmap(transformer)(_ + 1).value,
      List(Just(11))
    )

    assertEquals(
      Functor[MaybeT[List, *]].fmap(MaybeT(List(Just(10))))(_ + 1).value,
      List(Just(11))
    )

    assertEquals(
      Functor[MaybeT[List, *]].fmap(MaybeT(List(Nothing[Int])))(_ + 1).value,
      List(Nothing[Int])
    )

    assertEquals(
      Functor[MaybeT[List, *]].fmap(MaybeT(List.empty[Maybe[Int]]))(_ + 1).value,
      Nil
    )
  }

  property("MaybeT Functor syntax") {
    import tech.backwards.fp.learn.Functor.syntax._
    import tech.backwards.fp.learn.Maybe.syntax._

    val transformer: MaybeT[List, Int] =
      MaybeT(List(10.just))

    assertEquals(
      transformer.fmap(_ + 1).value,
      List(11.just)
    )

    assertEquals(
      MaybeT(List(10.just)).fmap(_ + 1).value,
      List(11.just)
    )

    assertEquals(
      MaybeT(List(10.just)) fmap (_ + 1),
      MaybeT(List(11.just))
    )

    assertEquals(
      MaybeT(List(10.just)) `<$>` (_ + 1),
      MaybeT(List(11.just))
    )

    assertEquals(
      MaybeT(List(nothing[Int])) `<$>` (_ + 1),
      MaybeT(List(nothing[Int]))
    )

    assertEquals(
      MaybeT(List.empty[Maybe[Int]]) `<$>` (_ + 1),
      MaybeT(List.empty[Maybe[Int]])
    )
  }

  property("MaybeT Monad") {
    val transformer: MaybeT[List, Int] =
      Monad[MaybeT[List, *]].pure(10)

    assertEquals(
      Monad[MaybeT[List, *]].flatMap(transformer)(a => Monad[MaybeT[List, *]].pure(a + 1)).value,
      List(Just(11))
    )

    assertEquals(
      Monad[MaybeT[List, *]].flatMap(Monad[MaybeT[List, *]].pure(10))(a => Monad[MaybeT[List, *]].pure(a + 1)).value,
      List(Just(11))
    )

    assertEquals(
      Monad[MaybeT[List, *]].flatMap(MaybeT(List(Nothing[Int])))(a => Monad[MaybeT[List, *]].pure(a + 1)).value,
      List(Nothing[Int])
    )

    assertEquals(
      Monad[MaybeT[List, *]].flatMap(Monad[MaybeT[List, *]].pure(10))(_ => MaybeT(List(Nothing[Int]))).value,
      List(Nothing[Int])
    )

    assertEquals(
      Monad[MaybeT[List, *]].flatMap(MaybeT(List(Nothing[Int])))(_ => MaybeT(List(Nothing[Int]))).value,
      List(Nothing[Int])
    )

    assertEquals(
      Monad[MaybeT[List, *]].flatMap(MaybeT(List.empty[Maybe[Int]]))(_ => MaybeT(List(Nothing[Int]))).value,
      Nil
    )

    assertEquals(
      Monad[MaybeT[List, *]].flatMap(MaybeT(List(Nothing[Int])))(_ => MaybeT(List.empty[Maybe[Int]])).value,
      List(Nothing[Int])
    )
  }

  property("MaybeT Monad syntax") {
    import tech.backwards.fp.learn.Monad.syntax._

    val transformer: MaybeT[List, Int] =
      10.pure[MaybeT[List, *]]

    assertEquals(
      transformer.flatMap(a => (a + 1).pure[MaybeT[List, *]]).value,
      List(Just(11))
    )

    assertEquals(
      10.pure[MaybeT[List, *]].flatMap(a => (a + 1).pure[MaybeT[List, *]]).value,
      List(Just(11))
    )

    assertEquals(
      MaybeT(List(Nothing[Int])).flatMap(a => (a + 1).pure[MaybeT[List, *]]).value,
      List(Nothing[Int])
    )

    assertEquals(
      10.pure[MaybeT[List, *]].flatMap(_ => MaybeT(List(Nothing[Int]))).value,
      List(Nothing[Int])
    )

    assertEquals(
      MaybeT(List(Nothing[Int])).flatMap(_ => MaybeT(List(Nothing[Int]))).value,
      List(Nothing[Int])
    )

    assertEquals(
      MaybeT(List.empty[Maybe[Int]]).flatMap(_ => MaybeT(List(Nothing[Int]))).value,
      Nil
    )

    assertEquals(
      MaybeT(List(Nothing[Int])).flatMap(_ => MaybeT(List.empty[Maybe[Int]])).value,
      List(Nothing[Int])
    )
  }

  property("MaybeT Applicative") {
    val transformerFn: MaybeT[List, Int => Int] =
      Applicative[MaybeT[List, *]].pure(_ + 1)

    val transformer: MaybeT[List, Int] =
      Applicative[MaybeT[List, *]].pure(10)

    assertEquals(
      Applicative[MaybeT[List, *]].ap(transformerFn)(transformer).value,
      List(Just(11))
    )

    assertEquals(
      Applicative[MaybeT[List, *]].ap(Applicative[MaybeT[List, *]].pure((x: Int) => x + 1))(Applicative[MaybeT[List, *]].pure(10)).value,
      List(Just(11))
    )

    assertEquals(
      Applicative[MaybeT[List, *]].ap(MaybeT(List(Nothing[Int => Int])))(Applicative[MaybeT[List, *]].pure(10)).value,
      List(Nothing[Int])
    )

    assertEquals(
      Applicative[MaybeT[List, *]].ap(MaybeT(List.empty[Maybe[Int => Int]]))(Applicative[MaybeT[List, *]].pure(10)).value,
      Nil
    )

    assertEquals(
      Applicative[MaybeT[List, *]].ap(Applicative[MaybeT[List, *]].pure((x: Int) => x + 1))(MaybeT(List(Nothing[Int]))).value,
      List(Nothing[Int])
    )

    assertEquals(
      Applicative[MaybeT[List, *]].ap(Applicative[MaybeT[List, *]].pure((x: Int) => x + 1))(MaybeT(List.empty[Maybe[Int]])).value,
      Nil
    )
  }

  property("MaybeT Applicative syntax") {
    import tech.backwards.fp.learn.Applicative.syntax._

    val transformerFn: MaybeT[List, Int => Int] =
      ((x: Int) => x + 1).pure[MaybeT[List, *]]

    val transformer: MaybeT[List, Int] =
      10.pure[MaybeT[List, *]]

    assertEquals(
      transformerFn.ap(transformer).value,
      List(Just(11))
    )

    assertEquals(
      ((x: Int) => x + 1).pure[MaybeT[List, *]].ap(10.pure[MaybeT[List, *]]).value,
      List(Just(11))
    )

    assertEquals(
      MaybeT(List(Nothing[Int => Int])).ap(10.pure[MaybeT[List, *]]).value,
      List(Nothing[Int])
    )

    assertEquals(
      MaybeT(List.empty[Maybe[Int => Int]]).ap(10.pure[MaybeT[List, *]]).value,
      Nil
    )

    assertEquals(
      ((x: Int) => x + 1).pure[MaybeT[List, *]].ap(MaybeT(List(Nothing[Int]))).value,
      List(Nothing[Int])
    )

    assertEquals(
      ((x: Int) => x + 1).pure[MaybeT[List, *]].ap(MaybeT(List.empty[Maybe[Int]])).value,
      Nil
    )
  }

  property("MaybeT Functor and Applicative syntax") {
    import tech.backwards.fp.learn.Applicative.syntax._
    import tech.backwards.fp.learn.Functor.syntax._
    import tech.backwards.fp.learn.Maybe.syntax._

    assertEquals(
      MaybeT(List(10.just)) `<$>` (x => (y: Int) => x + y) <*> MaybeT(List(1.just)),
      MaybeT(List(11.just))
    )

    assertEquals(
      MaybeT(List(nothing[Int])) `<$>` (x => (y: Int) => x + y) <*> MaybeT(List(1.just)),
      MaybeT(List(nothing[Int]))
    )

    assertEquals(
      MaybeT(List.empty[Maybe[Int]]) `<$>` (x => (y: Int) => x + y) <*> MaybeT(List(1.just)),
      MaybeT(List.empty[Maybe[Int]])
    )

    assertEquals(
      MaybeT(List(10.just)) `<$>` (x => (y: Int) => x + y) <*> MaybeT(List(nothing[Int])),
      MaybeT(List(nothing[Int]))
    )

    assertEquals(
      MaybeT(List(10.just)) `<$>` (x => (y: Int) => x + y) <*> MaybeT(List.empty[Maybe[Int]]),
      MaybeT(List.empty[Maybe[Int]])
    )

    val add: Int => Int => Int =
      x => y => x + y

    assertEquals(
      MaybeT(List(10.just)) `<$>` add <*> MaybeT(List(nothing[Int])),
      MaybeT(List(nothing[Int]))
    )
  }

  property("MaybeT Functor and Applicative function syntax") {
    import tech.backwards.fp.learn.Applicative.syntax._
    import tech.backwards.fp.learn.Functor.syntax.function._
    import tech.backwards.fp.learn.Maybe.syntax._

    assertEquals(
      ((x: Int) => (y: Int) => x + y) `<$>` MaybeT(List(10.just)) <*> MaybeT(List(1.just)),
      MaybeT(List(11.just))
    )

    assertEquals(
      ((x: Int) => (y: Int) => x + y) `<$>` MaybeT(List(nothing[Int])) <*> MaybeT(List(1.just)),
      MaybeT(List(nothing[Int]))
    )

    assertEquals(
      ((x: Int) => (y: Int) => x + y) `<$>` MaybeT(List.empty[Maybe[Int]]) <*> MaybeT(List(1.just)),
      MaybeT(List.empty[Maybe[Int]])
    )

    assertEquals(
      ((x: Int) => (y: Int) => x + y) `<$>` MaybeT(List(10.just)) <*> MaybeT(List(nothing[Int])),
      MaybeT(List(nothing[Int]))
    )

    assertEquals(
      ((x: Int) => (y: Int) => x + y) `<$>` MaybeT(List(10.just)) <*> MaybeT(List.empty[Maybe[Int]]),
      MaybeT(List.empty[Maybe[Int]])
    )

    val add: Int => Int => Int =
      x => y => x + y

    assertEquals(
      add `<$>` MaybeT(List(10.just)) <*> MaybeT(List(1.just)),
      MaybeT(List(11.just))
    )
  }

  property("MaybeT for comprehension") {
    import tech.backwards.fp.learn.Functor.syntax._
    import tech.backwards.fp.learn.Maybe.syntax._
    import tech.backwards.fp.learn.Monad.syntax._

    val transformer: MaybeT[List, Int] =
      for {
        x <- 10.pure[MaybeT[List, *]]
        y <- 11.pure[MaybeT[List, *]]
        z <- 12.pure[MaybeT[List, *]]
      } yield x + y + z

    assertEquals(
      transformer.value,
      List(Just(33))
    )

    assertEquals(
      for {
        x <- MaybeT.lift(List(10))
        y <- MaybeT.lift(List(11))
        z <- MaybeT.lift(List(12))
        _ <- MaybeT(List(0).map(Just.apply)) // Without "lift"
      } yield x + y + z,
      MaybeT(List(Just(33)))
    )

    assertEquals(
      for {
        x <- MaybeT.lift(List(10))
        y <- MaybeT(List(nothing[Int]))
        z <- MaybeT.lift(List(12))
      } yield x + y + z,
      MaybeT(List(nothing[Int]))
    )

    assertEquals(
      for {
        x <- MaybeT.lift(List(10))
        y <- MaybeT(List.empty[Maybe[Int]])
        z <- MaybeT.lift(List(12))
      } yield x + y + z,
      MaybeT(List.empty[Maybe[Int]])
    )
  }
}