package tech.backwards.fp.learn

import munit.ScalaCheckSuite
import org.scalacheck.Test

class DisjunctionMonadTransformerListSuite extends ScalaCheckSuite {
  override protected def scalaCheckTestParameters: Test.Parameters =
    super.scalaCheckTestParameters.withMinSuccessfulTests(100).withMaxDiscardRatio(10)

  /**
   * List[String Disjuction Int]
   */
  property("DisjunctionT") {
    val transformer: DisjunctionT[List, String, Int] =
      DisjunctionT(List(Right[String, Int](10)))

    assertEquals(
      transformer.value,
      List(Right[String, Int](10))
    )

    assertEquals(
      DisjunctionT(List(Left[String, Int]("whoops"))).value,
      List(Left[String, Int]("whoops"))
    )

    assertEquals(
      DisjunctionT(List.empty[String Disjunction Int]).value,
      Nil
    )
  }

  property("DisjunctionT pure") {
    val transformer: DisjunctionT[List, String, Int] =
      DisjunctionT.pure[List, String, Int](10)

    assertEquals(
      transformer.value,
      List(Right[String, Int](10))
    )
  }

  property("DisjunctionT lift") {
    val transformer: DisjunctionT[List, String, Int] =
      DisjunctionT.lift(List(10))

    assertEquals(
      transformer.value,
      List(Right(10))
    )

    assertEquals(
      DisjunctionT.lift(List(10)).value,
      List(Right(10))
    )

    assertEquals(
      DisjunctionT.lift(List.empty[String Disjunction Int]).value,
      Nil
    )
  }

  property("DisjunctionT Functor") {
    val transformer: DisjunctionT[List, String, Int] =
      DisjunctionT(List(Right(10)))

    assertEquals(
      Functor[DisjunctionT[List, String, *]].fmap(transformer)(_ + 1).value,
      List(Right(11))
    )

    assertEquals(
      Functor[DisjunctionT[List, String, *]].fmap(DisjunctionT(List(Right(10))))(_ + 1).value,
      List(Right(11))
    )

    assertEquals(
      Functor[DisjunctionT[List, String, *]].fmap(DisjunctionT(List(Left[String, Int]("whoops"))))(_ + 1).value,
      List(Left("whoops"))
    )

    assertEquals(
      Functor[DisjunctionT[List, String, *]].fmap(DisjunctionT(List.empty[String Disjunction Int]))(_ + 1).value,
      Nil
    )
  }

  property("DisjunctionT Functor syntax") {
    import tech.backwards.fp.learn.Disjunction.syntax._
    import tech.backwards.fp.learn.Functor.syntax._

    val transformer: DisjunctionT[List, String, Int] =
      DisjunctionT(List(10.right))

    assertEquals(
      transformer.fmap(_ + 1).value,
      List(11.right)
    )

    assertEquals(
      DisjunctionT(List(10.right[String])).fmap(_ + 1).value,
      List(11.right)
    )

    assertEquals(
      DisjunctionT(List(10.right[String])) fmap (_ + 1),
      DisjunctionT(List(11.right[String]))
    )

    assertEquals(
      DisjunctionT(List(10.right[String])) `<$>` (_ + 1),
      DisjunctionT(List(11.right[String]))
    )

    assertEquals(
      DisjunctionT(List("whoops".left[Int])) `<$>` (_ + 1),
      DisjunctionT(List("whoops".left[Int]))
    )

    assertEquals(
      DisjunctionT(List.empty[String Disjunction Int]) `<$>` (_ + 1),
      DisjunctionT(List.empty[String Disjunction Int])
    )
  }

  property("DisjunctionT Monad") {
    val transformer: DisjunctionT[List, String, Int] =
      Monad[DisjunctionT[List, String, *]].pure(10)

    assertEquals(
      Monad[DisjunctionT[List, String, *]].flatMap(transformer)(a => Monad[DisjunctionT[List, String, *]].pure(a + 1)).value,
      List(Right(11))
    )

    assertEquals(
      Monad[DisjunctionT[List, String, *]].flatMap(Monad[DisjunctionT[List, String, *]].pure(10))(a => Monad[DisjunctionT[List, String, *]].pure(a + 1)).value,
      List(Right(11))
    )

    assertEquals(
      Monad[DisjunctionT[List, String, *]].flatMap(DisjunctionT(List(Left[String, Int]("whoops"))))(a => Monad[DisjunctionT[List, String, *]].pure(a + 1)).value,
      List(Left("whoops"))
    )

    assertEquals(
      Monad[DisjunctionT[List, String, *]].flatMap(Monad[DisjunctionT[List, String, *]].pure(10))(_ => DisjunctionT(List(Left[String, Int]("whoops")))).value,
      List(Left("whoops"))
    )

    assertEquals(
      Monad[DisjunctionT[List, String, *]].flatMap(DisjunctionT(List(Left[String, Int]("whoops"))))(_ => DisjunctionT(List(Left[String, Int]("whoops")))).value,
      List(Left("whoops"))
    )

    assertEquals(
      Monad[DisjunctionT[List, String, *]].flatMap(DisjunctionT(List.empty[String Disjunction Int]))(_ => DisjunctionT(List(Left[String, Int]("whoops")))).value,
      Nil
    )

    assertEquals(
      Monad[DisjunctionT[List, String, *]].flatMap(DisjunctionT(List(Left[String, Int]("whoops"))))(_ => DisjunctionT(List.empty[String Disjunction Int])).value,
      List(Left("whoops"))
    )
  }

  property("DisjunctionT Monad syntax") {
    import tech.backwards.fp.learn.Monad.syntax._

    val transformer: DisjunctionT[List, String, Int] =
      10.pure[DisjunctionT[List, String, *]]

    assertEquals(
      transformer.flatMap(a => (a + 1).pure[DisjunctionT[List, String, *]]).value,
      List(Right(11))
    )

    assertEquals(
      10.pure[DisjunctionT[List, String, *]].flatMap(a => (a + 1).pure[DisjunctionT[List, String, *]]).value,
      List(Right(11))
    )

    assertEquals(
      DisjunctionT(List(Left[String, Int]("whoops"))).flatMap(a => (a + 1).pure[DisjunctionT[List, String, *]]).value,
      List(Left("whoops"))
    )

    assertEquals(
      10.pure[DisjunctionT[List, String, *]].flatMap(_ => DisjunctionT(List(Left[String, Int]("whoops")))).value,
      List(Left("whoops"))
    )

    assertEquals(
      DisjunctionT(List(Left[String, Int]("whoops"))).flatMap(_ => DisjunctionT(List(Left[String, Int]("whoops")))).value,
      List(Left("whoops"))
    )

    assertEquals(
      DisjunctionT(List.empty[String Disjunction Int]).flatMap(_ => DisjunctionT(List(Left[String, Int]("whoops")))).value,
      Nil
    )

    assertEquals(
      DisjunctionT(List(Left[String, Int]("whoops"))).flatMap(_ => DisjunctionT(List.empty[String Disjunction Int])).value,
      List(Left("whoops"))
    )
  }

  property("DisjunctionT Applicative") {
    val transformerFn: DisjunctionT[List, String, Int => Int] =
      Applicative[DisjunctionT[List, String, *]].pure(_ + 1)

    val transformer: DisjunctionT[List, String, Int] =
      Applicative[DisjunctionT[List, String, *]].pure(10)

    assertEquals(
      Applicative[DisjunctionT[List, String, *]].ap(transformerFn)(transformer).value,
      List(Right(11))
    )

    assertEquals(
      Applicative[DisjunctionT[List, String, *]].ap(Applicative[DisjunctionT[List, String, *]].pure((x: Int) => x + 1))(Applicative[DisjunctionT[List, String, *]].pure(10)).value,
      List(Right(11))
    )

    assertEquals(
      Applicative[DisjunctionT[List, String, *]].ap(DisjunctionT(List(Left[String, Int => Int]("whoops"))))(Applicative[DisjunctionT[List, String, *]].pure(10)).value,
      List(Left("whoops"))
    )

    assertEquals(
      Applicative[DisjunctionT[List, String, *]].ap(DisjunctionT(List.empty[String Disjunction (Int => Int)]))(Applicative[DisjunctionT[List, String, *]].pure(10)).value,
      Nil
    )

    assertEquals(
      Applicative[DisjunctionT[List, String, *]].ap(Applicative[DisjunctionT[List, String, *]].pure((x: Int) => x + 1))(DisjunctionT(List(Left[String, Int]("whoops")))).value,
      List(Left("whoops"))
    )

    assertEquals(
      Applicative[DisjunctionT[List, String, *]].ap(Applicative[DisjunctionT[List, String, *]].pure((x: Int) => x + 1))(DisjunctionT(List.empty[String Disjunction Int])).value,
      Nil
    )
  }

  property("DisjunctionT Applicative syntax") {
    import tech.backwards.fp.learn.Applicative.syntax._

    val transformerFn: DisjunctionT[List, String, Int => Int] =
      ((x: Int) => x + 1).pure[DisjunctionT[List, String, *]]

    val transformer: DisjunctionT[List, String, Int] =
      10.pure[DisjunctionT[List, String, *]]

    assertEquals(
      transformerFn.ap(transformer).value,
      List(Right(11))
    )

    assertEquals(
      ((x: Int) => x + 1).pure[DisjunctionT[List, String, *]].ap(10.pure[DisjunctionT[List, String, *]]).value,
      List(Right(11))
    )

    assertEquals(
      DisjunctionT(List(Left[String, Int => Int]("whoops"))).ap(10.pure[DisjunctionT[List, String, *]]).value,
      List(Left("whoops"))
    )

    assertEquals(
      DisjunctionT(List.empty[String Disjunction (Int => Int)]).ap(10.pure[DisjunctionT[List, String, *]]).value,
      Nil
    )

    assertEquals(
      ((x: Int) => x + 1).pure[DisjunctionT[List, String, *]].ap(DisjunctionT(List(Left("whoops")))).value,
      List(Left("whoops"))
    )

    assertEquals(
      ((x: Int) => x + 1).pure[DisjunctionT[List, String, *]].ap(DisjunctionT(List.empty[String Disjunction Int])).value,
      Nil
    )
  }

  property("DisjunctionT Functor and Applicative syntax") {
    import tech.backwards.fp.learn.Applicative.syntax._
    import tech.backwards.fp.learn.Disjunction.syntax._
    import tech.backwards.fp.learn.Functor.syntax._

    assertEquals(
      DisjunctionT(List(10.right[String])) `<$>` (x => (y: Int) => x + y) <*> DisjunctionT(List(1.right)),
      DisjunctionT(List(11.right[String]))
    )

    assertEquals(
      DisjunctionT(List("whoops".left[Int])) `<$>` (x => (y: Int) => x + y) <*> DisjunctionT(List(1.right)),
      DisjunctionT(List("whoops".left[Int]))
    )

    assertEquals(
      DisjunctionT(List.empty[String Disjunction Int]) `<$>` (x => (y: Int) => x + y) <*> DisjunctionT(List(1.right)),
      DisjunctionT(List.empty[String Disjunction Int])
    )

    assertEquals(
      DisjunctionT(List(10.right[String])) `<$>` (x => (y: Int) => x + y) <*> DisjunctionT(List("whoops".left)),
      DisjunctionT(List("whoops".left[Int]))
    )

    assertEquals(
      DisjunctionT(List(10.right[String])) `<$>` (x => (y: Int) => x + y) <*> DisjunctionT(List.empty[String Disjunction Int]),
      DisjunctionT(List.empty[String Disjunction Int])
    )

    val add: Int => Int => Int =
      x => y => x + y

    assertEquals(
      DisjunctionT(List(10.right[String])) `<$>` add <*> DisjunctionT(List("whoops".left)),
      DisjunctionT(List("whoops".left[Int]))
    )
  }

  property("DisjunctionT Functor and Applicative function syntax") {
    import tech.backwards.fp.learn.Applicative.syntax._
    import tech.backwards.fp.learn.Disjunction.syntax._
    import tech.backwards.fp.learn.Functor.syntax.function._

    assertEquals(
      ((x: Int) => (y: Int) => x + y) `<$>` DisjunctionT(List(10.right)) <*> DisjunctionT(List(1.right)),
      DisjunctionT(List(11.right))
    )

    assertEquals(
      ((x: Int) => (y: Int) => x + y) `<$>` DisjunctionT(List("whoops".left[Int])) <*> DisjunctionT(List(1.right)),
      DisjunctionT(List("whoops".left[Int]))
    )

    assertEquals(
      ((x: Int) => (y: Int) => x + y) `<$>` DisjunctionT(List.empty[String Disjunction Int]) <*> DisjunctionT(List(1.right)),
      DisjunctionT(List.empty[String Disjunction Int])
    )

    assertEquals(
      ((x: Int) => (y: Int) => x + y) `<$>` DisjunctionT(List(10.right[String])) <*> DisjunctionT(List("whoops".left[Int])),
      DisjunctionT(List("whoops".left[Int]))
    )

    assertEquals(
      ((x: Int) => (y: Int) => x + y) `<$>` DisjunctionT(List(10.right[String])) <*> DisjunctionT(List.empty[String Disjunction Int]),
      DisjunctionT(List.empty[String Disjunction Int])
    )

    val add: Int => Int => Int =
      x => y => x + y

    assertEquals(
      add `<$>` DisjunctionT(List(10.right)) <*> DisjunctionT(List(1.right)),
      DisjunctionT(List(11.right))
    )
  }

  property("DisjunctionT for comprehension") {
    import tech.backwards.fp.learn.Disjunction.syntax._
    import tech.backwards.fp.learn.Functor.syntax._
    import tech.backwards.fp.learn.Monad.syntax._

    val transformer: DisjunctionT[List, String, Int] =
      for {
        x <- 10.pure[DisjunctionT[List, String, *]]
        y <- 11.pure[DisjunctionT[List, String, *]]
        z <- 12.pure[DisjunctionT[List, String, *]]
      } yield x + y + z

    assertEquals(
      transformer.value,
      List(Right(33))
    )

    assertEquals(
      for {
        x <- DisjunctionT.lift[List, String, Int](List(10))
        y <- DisjunctionT.lift[List, String, Int](List(11))
        z <- DisjunctionT.lift[List, String, Int](List(12))
        _ <- DisjunctionT(List(0).map(Right.apply[String, Int])) // Without "lift"
      } yield x + y + z,
      DisjunctionT(List(Right[String, Int](33)))
    )

    assertEquals(
      for {
        x <- DisjunctionT.lift[List, String, Int](List(10))
        y <- DisjunctionT(List("whoops".left[Int]))
        z <- DisjunctionT.lift[List, String, Int](List(12))
      } yield x + y + z,
      DisjunctionT(List("whoops".left[Int]))
    )

    assertEquals(
      for {
        x <- DisjunctionT.lift[List, String, Int](List(10))
        y <- DisjunctionT(List.empty[String Disjunction Int])
        z <- DisjunctionT.lift[List, String, Int](List(12))
      } yield x + y + z,
      DisjunctionT(List.empty[String Disjunction Int])
    )
  }
}