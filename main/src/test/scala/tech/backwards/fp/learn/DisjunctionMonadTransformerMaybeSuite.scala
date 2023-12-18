package tech.backwards.fp.learn

import munit.ScalaCheckSuite
import org.scalacheck.Test

class DisjunctionMonadTransformerMaybeSuite extends ScalaCheckSuite {
  override protected def scalaCheckTestParameters: Test.Parameters =
    super.scalaCheckTestParameters.withMinSuccessfulTests(100).withMaxDiscardRatio(10)

  property("DisjunctionT") {
    val transformer: DisjunctionT[Maybe, String, Int] =
      DisjunctionT(Just(Right[String, Int](10)))

    assertEquals(
      transformer.value,
      Just(Right[String, Int](10))
    )

    assertEquals(
      DisjunctionT(Just(Right[String, Int](10))).value,
      Just(Right[String, Int](10))
    )

    assertEquals(
      DisjunctionT(Just(Left[String, Int]("whoops"))).value,
      Just(Left[String, Int]("whoops"))
    )

    assertEquals(
      DisjunctionT(Nothing[String Disjunction Int]).value,
      Nothing[String Disjunction Int]
    )
  }

  property("DisjunctionT pure") {
    val transformer: DisjunctionT[Maybe, String, Int] =
      DisjunctionT.pure[Maybe, String, Int](10)

    assertEquals(
      transformer.value,
      Just(Right[String, Int](10))
    )
  }

  property("DisjunctionT lift") {
    val transformer: DisjunctionT[Maybe, String, Int] =
      DisjunctionT.lift(Just(10))

    assertEquals(
      transformer.value,
      Just(Right[String, Int](10))
    )

    assertEquals(
      DisjunctionT.lift(Just(10)).value,
      Just(Right[String, Int](10))
    )

    assertEquals(
      DisjunctionT.lift(Nothing[Int]).value,
      Nothing[String Disjunction Int]
    )
  }

  property("DisjunctionT Functor") {
    val transformer: DisjunctionT[Maybe, String, Int] =
      DisjunctionT(Just(Right[String, Int](10)))

    assertEquals(
      Functor[DisjunctionT[Maybe, String, *]].fmap(transformer)(_ + 1).value,
      Just(Right[String, Int](11))
    )

    assertEquals(
      Functor[DisjunctionT[Maybe, String, *]].fmap(DisjunctionT(Just(Right[String, Int](10))))(_ + 1).value,
      Just(Right[String, Int](11))
    )

    assertEquals(
      Functor[DisjunctionT[Maybe, String, *]].fmap(DisjunctionT(Just(Left[String, Int]("whoops"))))(_ + 1).value,
      Just(Left[String, Int]("whoops"))
    )

    assertEquals(
      Functor[DisjunctionT[Maybe, String, *]].fmap(DisjunctionT(Nothing[String Disjunction Int]))(_ + 1).value,
      Nothing[String Disjunction Int]
    )
  }

  property("DisjunctionT Functor syntax") {
    import tech.backwards.fp.learn.Disjunction.syntax._
    import tech.backwards.fp.learn.Functor.syntax._
    import tech.backwards.fp.learn.Maybe.syntax._

    val transformer: DisjunctionT[Maybe, String, Int] =
      DisjunctionT(10.right.just)

    assertEquals(
      transformer.fmap(_ + 1).value,
      11.right.just
    )

    assertEquals(
      DisjunctionT(10.right[String].just).fmap(_ + 1).value,
      11.right[String].just
    )

    assertEquals(
      DisjunctionT(10.right[String].just) fmap (_ + 1),
      DisjunctionT(11.right[String].just)
    )

    assertEquals(
      DisjunctionT(10.right[String].just) `<$>` (_ + 1),
      DisjunctionT(11.right[String].just)
    )

    assertEquals(
      DisjunctionT("whoops".left[Int].just) `<$>` (_ + 1),
      DisjunctionT("whoops".left[Int].just)
    )

    assertEquals(
      DisjunctionT(nothing[String Disjunction Int]) `<$>` (_ + 1),
      DisjunctionT(nothing[String Disjunction Int])
    )
  }

  property("DisjunctionT Monad") {
    val transformer: DisjunctionT[Maybe, String, Int] =
      Monad[DisjunctionT[Maybe, String, *]].pure(10)

    assertEquals(
      Monad[DisjunctionT[Maybe, String, *]].flatMap(transformer)(a => Monad[DisjunctionT[Maybe, String, *]].pure(a + 1)).value,
      Just(Right[String, Int](11))
    )

    assertEquals(
      Monad[DisjunctionT[Maybe, String, *]].flatMap(Monad[DisjunctionT[Maybe, String, *]].pure(10))(a => Monad[DisjunctionT[Maybe, String, *]].pure(a + 1)).value,
      Just(Right[String, Int](11))
    )

    assertEquals(
      Monad[DisjunctionT[Maybe, String, *]].flatMap(DisjunctionT(Just(Left[String, Int]("whoops"))))(a => Monad[DisjunctionT[Maybe, String, *]].pure(a + 1)).value,
      Just(Left[String, Int]("whoops"))
    )

    assertEquals(
      Monad[DisjunctionT[Maybe, String, *]].flatMap(Monad[DisjunctionT[Maybe, String, *]].pure(10))(_ => DisjunctionT(Just(Left[String, Int]("whoops")))).value,
      Just(Left[String, Int]("whoops"))
    )

    assertEquals(
      Monad[DisjunctionT[Maybe, String, *]].flatMap(DisjunctionT(Just(Left[String, Int]("whoops"))))(_ => DisjunctionT(Just(Left[String, Int]("I did it again")))).value,
      Just(Left[String, Int]("whoops"))
    )

    assertEquals(
      Monad[DisjunctionT[Maybe, String, *]].flatMap(DisjunctionT(Nothing[String Disjunction Int]))(_ => DisjunctionT(Just(Left[String, Int]("whoops")))).value,
      Nothing[String Disjunction Int]
    )

    assertEquals(
      Monad[DisjunctionT[Maybe, String, *]].flatMap(DisjunctionT(Just(Left[String, Int]("whoops"))))(_ => DisjunctionT(Nothing[String Disjunction Int])).value,
      Just(Left[String, Int]("whoops"))
    )
  }

  property("DisjunctionT Monad syntax") {
    import tech.backwards.fp.learn.Disjunction.syntax._
    import tech.backwards.fp.learn.Monad.syntax._

    val transformer: DisjunctionT[Maybe, String, Int] =
      10.pure[DisjunctionT[Maybe, String, *]]

    assertEquals(
      transformer.flatMap(a => (a + 1).pure[DisjunctionT[Maybe, String, *]]).value,
      Just(11.right)
    )

    assertEquals(
      10.pure[DisjunctionT[Maybe, String, *]].flatMap(a => (a + 1).pure[DisjunctionT[Maybe, String, *]]).value,
      Just(11.right)
    )

    assertEquals(
      DisjunctionT(Just("whoops".left[Int])).flatMap(a => (a + 1).pure[DisjunctionT[Maybe, String, *]]).value,
      Just("whoops".left[Int])
    )

    assertEquals(
      10.pure[DisjunctionT[Maybe, String, *]].flatMap(_ => DisjunctionT(Just("whoops".left[Int]))).value,
      Just("whoops".left[Int])
    )

    assertEquals(
      DisjunctionT(Just("whoops".left[Int])).flatMap(_ => DisjunctionT(Just("whoops".left[Int]))).value,
      Just("whoops".left[Int])
    )

    assertEquals(
      DisjunctionT(Nothing[String Disjunction Int]).flatMap(_ => DisjunctionT(Just("whoops".left[Int]))).value,
      Nothing[String Disjunction Int]
    )

    assertEquals(
      DisjunctionT(Just("whoops".left[Int])).flatMap(_ => DisjunctionT(Nothing[String Disjunction Int])).value,
      Just("whoops".left[Int])
    )
  }

  property("DisjunctionT Applicative") {
    val transformerFn: DisjunctionT[Maybe, String, Int => Int] =
      Applicative[DisjunctionT[Maybe, String, *]].pure(_ + 1)

    val transformer: DisjunctionT[Maybe, String, Int] =
      Applicative[DisjunctionT[Maybe, String, *]].pure(10)

    assertEquals(
      Applicative[DisjunctionT[Maybe, String, *]].ap(transformerFn)(transformer).value,
      Just(Right[String, Int](11))
    )

    assertEquals(
      Applicative[DisjunctionT[Maybe, String, *]].ap(Applicative[DisjunctionT[Maybe, String, *]].pure((x: Int) => x + 1))(Applicative[DisjunctionT[Maybe, String, *]].pure(10)).value,
      Just(Right[String, Int](11))
    )

    assertEquals(
      Applicative[DisjunctionT[Maybe, String, *]].ap(DisjunctionT(Just(Left[String, Int => Int]("whoops"))))(Applicative[DisjunctionT[Maybe, String, *]].pure(10)).value,
      Just(Left[String, Int]("whoops"))
    )

    assertEquals(
      Applicative[DisjunctionT[Maybe, String, *]].ap(DisjunctionT(Nothing[String Disjunction (Int => Int)]))(Applicative[DisjunctionT[Maybe, String, *]].pure(10)).value,
      Nothing[String Disjunction Int]
    )

    assertEquals(
      Applicative[DisjunctionT[Maybe, String, *]].ap(Applicative[DisjunctionT[Maybe, String, *]].pure((x: Int) => x + 1))(DisjunctionT(Just(Left[String, Int]("whoops")))).value,
      Just(Left[String, Int]("whoops"))
    )

    assertEquals(
      Applicative[DisjunctionT[Maybe, String, *]].ap(Applicative[DisjunctionT[Maybe, String, *]].pure((x: Int) => x + 1))(DisjunctionT(Nothing[String Disjunction Int])).value,
      Nothing[String Disjunction Int]
    )
  }

  property("DisjunctionT Applicative syntax") {
    import tech.backwards.fp.learn.Applicative.syntax._
    import tech.backwards.fp.learn.Disjunction.syntax._

    val transformerFn: DisjunctionT[Maybe, String, Int => Int] =
      ((x: Int) => x + 1).pure[DisjunctionT[Maybe, String, *]]

    val transformer: DisjunctionT[Maybe, String, Int] =
      10.pure[DisjunctionT[Maybe, String, *]]

    assertEquals(
      transformerFn.ap(transformer).value,
      Just(11.right)
    )

    assertEquals(
      ((x: Int) => x + 1).pure[DisjunctionT[Maybe, String, *]].ap(10.pure[DisjunctionT[Maybe, String, *]]).value,
      Just(11.right)
    )

    assertEquals(
      DisjunctionT(Just("whoops".left[Int => Int])).ap(10.pure[DisjunctionT[Maybe, String, *]]).value,
      Just("whoops".left[Int])
    )

    assertEquals(
      DisjunctionT(Nothing[String Disjunction (Int => Int)]).ap(10.pure[DisjunctionT[Maybe, String, *]]).value,
      Nothing[String Disjunction Int]
    )

    assertEquals(
      ((x: Int) => x + 1).pure[DisjunctionT[Maybe, String, *]].ap(DisjunctionT(Just("whoops".left[Int]))).value,
      Just("whoops".left[Int])
    )

    assertEquals(
      ((x: Int) => x + 1).pure[DisjunctionT[Maybe, String, *]].ap(DisjunctionT(Nothing[String Disjunction Int])).value,
      Nothing[String Disjunction Int]
    )
  }

  property("DisjunctionT Functor and Applicative syntax") {
    import tech.backwards.fp.learn.Applicative.syntax._
    import tech.backwards.fp.learn.Disjunction.syntax._
    import tech.backwards.fp.learn.Functor.syntax._
    import tech.backwards.fp.learn.Maybe.syntax._

    assertEquals(
      DisjunctionT(10.right[String].just) `<$>` (x => (y: Int) => x + y) <*> DisjunctionT(1.right.just),
      DisjunctionT(11.right[String].just)
    )

    assertEquals(
      DisjunctionT("whoops".left[Int].just) `<$>` (x => (y: Int) => x + y) <*> DisjunctionT(1.right.just),
      DisjunctionT("whoops".left[Int].just)
    )

    assertEquals(
      DisjunctionT(Nothing[String Disjunction Int]) `<$>` (x => (y: Int) => x + y) <*> DisjunctionT(1.right.just),
      DisjunctionT(Nothing[String Disjunction Int])
    )

    assertEquals(
      DisjunctionT(10.right[String].just) `<$>` (x => (y: Int) => x + y) <*> DisjunctionT("whoops".left[Int].just),
      DisjunctionT("whoops".left[Int].just)
    )

    assertEquals(
      DisjunctionT(10.right[String].just) `<$>` (x => (y: Int) => x + y) <*> DisjunctionT(Nothing[String Disjunction Int]),
      DisjunctionT(Nothing[String Disjunction Int])
    )

    val add: Int => Int => Int =
      x => y => x + y

    assertEquals(
      DisjunctionT(10.right[String].just) `<$>` add <*> DisjunctionT("whoops".left[Int].just),
      DisjunctionT("whoops".left[Int].just)
    )
  }

  property("DisjunctionT Functor and Applicative function syntax") {
    import tech.backwards.fp.learn.Applicative.syntax._
    import tech.backwards.fp.learn.Disjunction.syntax._
    import tech.backwards.fp.learn.Functor.syntax.function._
    import tech.backwards.fp.learn.Maybe.syntax._

    assertEquals(
      ((x: Int) => (y: Int) => x + y) `<$>` DisjunctionT(10.right.just) <*> DisjunctionT(1.right.just),
      DisjunctionT(11.right.just)
    )

    assertEquals(
      ((x: Int) => (y: Int) => x + y) `<$>` DisjunctionT("whoops".left[Int].just) <*> DisjunctionT(1.right.just),
      DisjunctionT("whoops".left[Int].just)
    )

    assertEquals(
      ((x: Int) => (y: Int) => x + y) `<$>` DisjunctionT(Nothing[String Disjunction Int]) <*> DisjunctionT(1.right.just),
      DisjunctionT(Nothing[String Disjunction Int])
    )

    assertEquals(
      ((x: Int) => (y: Int) => x + y) `<$>` DisjunctionT(10.right[String].just) <*> DisjunctionT("whoops".left.just),
      DisjunctionT("whoops".left[Int].just)
    )

    assertEquals(
      ((x: Int) => (y: Int) => x + y) `<$>` DisjunctionT(10.right[String].just) <*> DisjunctionT(Nothing[String Disjunction Int]),
      DisjunctionT(Nothing[String Disjunction Int])
    )

    val add: Int => Int => Int =
      x => y => x + y

    assertEquals(
      add `<$>` DisjunctionT(10.right.just) <*> DisjunctionT(1.right.just),
      DisjunctionT(11.right.just)
    )
  }

  property("DisjunctionT for comprehension") {
    import tech.backwards.fp.learn.Disjunction.syntax._
    import tech.backwards.fp.learn.Functor.syntax._
    import tech.backwards.fp.learn.Maybe.syntax._
    import tech.backwards.fp.learn.Monad.syntax._

    val transformer: DisjunctionT[Maybe, String, Int] =
      for {
        x <- 10.pure[DisjunctionT[Maybe, String, *]]
        y <- 11.pure[DisjunctionT[Maybe, String, *]]
        z <- 12.pure[DisjunctionT[Maybe, String, *]]
      } yield x + y + z

    assertEquals(
      transformer.value,
      Just(Right(33))
    )

    assertEquals(
      for {
        x <- DisjunctionT.lift[Maybe, String, Int](Just(10))
        y <- DisjunctionT.lift[Maybe, String, Int](Just(11))
        z <- DisjunctionT.lift[Maybe, String, Int](Just(12))
        _ <- DisjunctionT(0.just.map(Right.apply[String, Int])) // Without "lift"
      } yield x + y + z,
      DisjunctionT(Just(33.right[String]))
    )

    assertEquals(
      for {
        x <- DisjunctionT.lift[Maybe, String, Int](Just(10))
        y <- DisjunctionT(Just("whoops".left[Int]))
        z <- DisjunctionT.lift[Maybe, String, Int](Just(12))
      } yield x + y + z,
      DisjunctionT(Just("whoops".left[Int]))
    )

    assertEquals(
      for {
        x <- DisjunctionT.lift[Maybe, String, Int](Just(10))
        y <- DisjunctionT(Nothing[String Disjunction Int])
        z <- DisjunctionT.lift[Maybe, String, Int](Just(12))
      } yield x + y + z,
      DisjunctionT(Nothing[String Disjunction Int])
    )
  }
}