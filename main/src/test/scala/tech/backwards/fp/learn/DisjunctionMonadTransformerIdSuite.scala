package tech.backwards.fp.learn

import munit.ScalaCheckSuite
import org.scalacheck.Test

/**
 * First of the following DisjunctionT suites:
 *  - DisjunctionMonadTransformerMaybeSuite
 *  - DisjunctionMonadTransformerDisjunctionSuite
 *  - DisjunctionMonadTransformerListSuite
 *  - DisjunctionMonadTransformerWriterSuite
 *  - DisjunctionMonadTransformerStateSuite
 */
class DisjunctionMonadTransformerIdSuite extends ScalaCheckSuite {
  override protected def scalaCheckTestParameters: Test.Parameters =
    super.scalaCheckTestParameters.withMinSuccessfulTests(100).withMaxDiscardRatio(10)

  property("DisjunctionT") {
    val transformer: DisjunctionT[Id, String, Int] =
      DisjunctionT(Id(Right[String, Int](10)))

    assertEquals(
      transformer.value,
      Id(Right[String, Int](10))
    )

    assertEquals(
      DisjunctionT(Id(Left[String, Int]("whoops"))).value,
      Id(Left[String, Int]("whoops"))
    )

    assertEquals(
      DisjunctionT(Id(Left[String, Int]("whoops"))).value.value,
      Left[String, Int]("whoops")
    )
  }

  property("DisjunctionT pure") {
    val transformer: DisjunctionT[Id, String, Int] =
      DisjunctionT.pure[Id, String, Int](10)

    assertEquals(
      transformer.value,
      Id(Right[String, Int](10))
    )
  }

  property("DisjunctionT lift") {
    val transformer: DisjunctionT[Id, String, Int] =
      DisjunctionT.lift(Id(10))

    assertEquals(
      transformer.value,
      Id(Right[String, Int](10))
    )

    assertEquals(
      DisjunctionT.lift[Id, String, Int](Id(10)).value,
      Id(Right[String, Int](10))
    )
  }

  property("DisjunctionT Functor") {
    val transformer: DisjunctionT[Id, String, Int] =
      DisjunctionT(Id(Right[String, Int](10)))

    assertEquals(
      Functor[DisjunctionT[Id, String, *]].fmap(transformer)(_ + 1).value,
      Id(Right[String, Int](11))
    )

    assertEquals(
      Functor[DisjunctionT[Id, String, *]].fmap(DisjunctionT(Id(Right[String, Int](10))))(_ + 1).value,
      Id(Right[String, Int](11))
    )

    assertEquals(
      Functor[DisjunctionT[Id, String, *]].fmap(DisjunctionT(Id(Left[String, Int]("whoops"))))(_ + 1).value,
      Id(Left[String, Int]("whoops"))
    )
  }

  property("DisjunctionT Functor syntax") {
    import tech.backwards.fp.learn.Functor.syntax._
    import tech.backwards.fp.learn.Disjunction.syntax._

    val transformer: DisjunctionT[Id, String, Int] =
      DisjunctionT(Id(10.right))

    assertEquals(
      transformer.fmap(_ + 1).value,
      Id(11.right[String])
    )

    assertEquals(
      DisjunctionT(Id(10.right[String])).fmap(_ + 1).value,
      Id(11.right[String])
    )

    assertEquals(
      DisjunctionT(Id(10.right[String])) fmap (_ + 1),
      DisjunctionT(Id(11.right[String]))
    )

    assertEquals(
      DisjunctionT(Id(10.right[String])) `<$>` (_ + 1),
      DisjunctionT(Id(11.right[String]))
    )

    assertEquals(
      DisjunctionT(Id("whoops".left[Int])) `<$>` (_ + 1),
      DisjunctionT(Id("whoops".left[Int]))
    )

    assertEquals(
      (DisjunctionT(Id("whoops".left[Int])) `<$>` (_ + 1)).value,
      Id("whoops".left[Int])
    )
  }

  property("DisjunctionT Monad") {
    val transformer: DisjunctionT[Id, String, Int] =
      Monad[DisjunctionT[Id, String, *]].pure(10)

    assertEquals(
      Monad[DisjunctionT[Id, String, *]].flatMap(transformer)(a => Monad[DisjunctionT[Id, String, *]].pure(a + 1)).value,
      Id(Right[String, Int](11))
    )

    assertEquals(
      Monad[DisjunctionT[Id, String, *]].flatMap(Monad[DisjunctionT[Id, String, *]].pure(10))(a => Monad[DisjunctionT[Id, String, *]].pure(a + 1)).value,
      Id(Right[String, Int](11))
    )

    assertEquals(
      Monad[DisjunctionT[Id, String, *]].flatMap(DisjunctionT(Id(Left[String, Int]("whoops"))))(a => Monad[DisjunctionT[Id, String, *]].pure(a + 1)).value,
      Id(Left[String, Int]("whoops"))
    )

    assertEquals(
      Monad[DisjunctionT[Id, String, *]].flatMap(Monad[DisjunctionT[Id, String, *]].pure(10))(_ => DisjunctionT(Id(Left[String, Int]("whoops")))).value,
      Id(Left[String, Int]("whoops"))
    )

    assertEquals(
      Monad[DisjunctionT[Id, String, *]].flatMap(DisjunctionT(Id(Left[String, Int]("whoops"))))(_ => DisjunctionT(Id(Left[String, Int]("I did it again")))).value,
      Id(Left[String, Int]("whoops"))
    )
  }

  property("DisjunctionT Monad syntax") {
    import tech.backwards.fp.learn.Disjunction.syntax._
    import tech.backwards.fp.learn.Monad.syntax._

    val transformer: DisjunctionT[Id, String, Int] =
      10.pure[DisjunctionT[Id, String, *]]

    assertEquals(
      transformer.flatMap(a => (a + 1).pure[DisjunctionT[Id, String, *]]).value,
      Id(11.right[String])
    )

    assertEquals(
      10.pure[DisjunctionT[Id, String, *]].flatMap(a => (a + 1).pure[DisjunctionT[Id, String, *]]).value,
      Id(11.right[String])
    )

    assertEquals(
      DisjunctionT(Id("whoops".left[Int])).flatMap(a => (a + 1).pure[DisjunctionT[Id, String, *]]).value,
      Id("whoops".left[Int])
    )

    assertEquals(
      10.pure[DisjunctionT[Id, String, *]].flatMap(_ => DisjunctionT(Id("whoops".left[Int]))).value,
      Id("whoops".left[Int])
    )

    assertEquals(
      DisjunctionT(Id("whoops".left[Int])).flatMap(_ => DisjunctionT(Id("I did it again".left[Int]))).value,
      Id("whoops".left[Int])
    )
  }

  property("DisjunctionT Applicative") {
    val transformerFn: DisjunctionT[Id, String, Int => Int] =
      Applicative[DisjunctionT[Id, String, *]].pure(_ + 1)

    val transformer: DisjunctionT[Id, String, Int] =
      Applicative[DisjunctionT[Id, String, *]].pure(10)

    assertEquals(
      Applicative[DisjunctionT[Id, String, *]].ap(transformerFn)(transformer).value,
      Id(Right[String, Int](11))
    )

    assertEquals(
      Applicative[DisjunctionT[Id, String, *]].ap(Applicative[DisjunctionT[Id, String, *]].pure((x: Int) => x + 1))(Applicative[DisjunctionT[Id, String, *]].pure(10)).value,
      Id(Right[String, Int](11))
    )

    assertEquals(
      Applicative[DisjunctionT[Id, String, *]].ap(DisjunctionT(Id(Left[String, Int => Int]("whoops"))))(Applicative[DisjunctionT[Id, String, *]].pure(10)).value,
      Id(Left[String, Int]("whoops"))
    )

    assertEquals(
      Applicative[DisjunctionT[Id, String, *]].ap(Applicative[DisjunctionT[Id, String, *]].pure((x: Int) => x + 1))(DisjunctionT(Id(Left[String, Int]("whoops")))).value,
      Id(Left[String, Int]("whoops"))
    )
  }

  property("DisjunctionT Applicative syntax") {
    import tech.backwards.fp.learn.Applicative.syntax._

    val transformerFn: DisjunctionT[Id, String, Int => Int] =
      ((x: Int) => x + 1).pure[DisjunctionT[Id, String, *]]

    val transformer: DisjunctionT[Id, String, Int] =
      10.pure[DisjunctionT[Id, String, *]]

    assertEquals(
      transformerFn.ap(transformer).value,
      Id(Right[String, Int](11))
    )

    assertEquals(
      ((x: Int) => x + 1).pure[DisjunctionT[Id, String, *]].ap(10.pure[DisjunctionT[Id, String, *]]).value,
      Id(Right[String, Int](11))
    )

    assertEquals(
      DisjunctionT(Id(Left[String, Int => Int]("whoops"))).ap(10.pure[DisjunctionT[Id, String, *]]).value,
      Id(Left[String, Int]("whoops"))
    )

    assertEquals(
      ((x: Int) => x + 1).pure[DisjunctionT[Id, String, *]].ap(DisjunctionT(Id(Left[String, Int]("whoops")))).value,
      Id(Left[String, Int]("whoops"))
    )

    assertEquals(
      ((x: Int) => x + 1).pure[DisjunctionT[Id, String, *]] <*> DisjunctionT(Id(Left[String, Int]("whoops"))),
      DisjunctionT(Id(Left[String, Int]("whoops")))
    )
  }

  property("DisjunctionT Functor and Applicative syntax") {
    import tech.backwards.fp.learn.Applicative.syntax._
    import tech.backwards.fp.learn.Disjunction.syntax._
    import tech.backwards.fp.learn.Functor.syntax._

    assertEquals(
      DisjunctionT(Id(Right[String, Int](10))) `<$>` (x => (y: Int) => x + y) <*> DisjunctionT(Id(Right[String, Int](1))),
      DisjunctionT(Id(Right[String, Int](11)))
    )

    assertEquals(
      DisjunctionT(Id(Left[String, Int]("whoops"))) `<$>` (x => (y: Int) => x + y) <*> DisjunctionT(Id(Right[String, Int](1))),
      DisjunctionT(Id(Left[String, Int]("whoops")))
    )

    assertEquals(
      DisjunctionT(Id(Right[String, Int](10))) `<$>` (x => (y: Int) => x + y) <*> DisjunctionT(Id(Left[String, Int]("whoops"))),
      DisjunctionT(Id(Left[String, Int]("whoops")))
    )

    val add: Int => Int => Int =
      x => y => x + y

    assertEquals(
      DisjunctionT(Id(Right[String, Int](10))) `<$>` add <*> DisjunctionT(Id(Left[String, Int]("whoops"))),
      DisjunctionT(Id(Left[String, Int]("whoops")))
    )
  }

  property("DisjunctionT Functor and Applicative function syntax") {
    import tech.backwards.fp.learn.Applicative.syntax._
    import tech.backwards.fp.learn.Disjunction.syntax._
    import tech.backwards.fp.learn.Functor.syntax.function._

    assertEquals(
      ((x: Int) => (y: Int) => x + y) `<$>` DisjunctionT(Id(10.right[String])) <*> DisjunctionT(Id(1.right[String])),
      DisjunctionT(Id(11.right[String]))
    )

    assertEquals(
      ((x: Int) => (y: Int) => x + y) `<$>` DisjunctionT(Id("whoops".left[Int])) <*> DisjunctionT(Id(1.right)),
      DisjunctionT(Id("whoops".left[Int]))
    )

    assertEquals(
      ((x: Int) => (y: Int) => x + y) `<$>` DisjunctionT(Id(10.right[String])) <*> DisjunctionT(Id("whoops".left)),
      DisjunctionT(Id("whoops".left[Int]))
    )

    val add: Int => Int => Int =
      x => y => x + y

    assertEquals(
      add `<$>` DisjunctionT(Id(10.right[String])) <*> DisjunctionT(Id(1.right)),
      DisjunctionT(Id(11.right[String]))
    )
  }

  property("DisjunctionT for comprehension") {
    import tech.backwards.fp.learn.Functor.syntax._
    import tech.backwards.fp.learn.Disjunction.syntax._
    import tech.backwards.fp.learn.Monad.syntax._

    val transformer: DisjunctionT[Id, String, Int] =
      for {
        x <- 10.pure[DisjunctionT[Id, String, *]]
        y <- 11.pure[DisjunctionT[Id, String, *]]
        z <- 12.pure[DisjunctionT[Id, String, *]]
      } yield x + y + z

    assertEquals(
      transformer.value,
      Id(33.right[String])
    )

    // TODO - Perhaps syntax for "lift"
    assertEquals(
      for {
        x <- DisjunctionT.lift[Id, String, Int](Id(10))
        y <- DisjunctionT.lift[Id, String, Int](Id(11))
        z <- DisjunctionT.lift[Id, String, Int](Id(12))
        _ <- DisjunctionT(Id(0).map(_.right[String])) // Without "lift"
      } yield x + y + z,
      DisjunctionT(Id(33.right[String]))
    )

    assertEquals(
      for {
        x <- DisjunctionT.lift[Id, String, Int](Id(10))
        y <- DisjunctionT(Id("whoops".left[Int]))
        z <- DisjunctionT.lift[Id, String, Int](Id(12))
      } yield x + y + z,
      DisjunctionT(Id("whoops".left[Int]))
    )
  }
}