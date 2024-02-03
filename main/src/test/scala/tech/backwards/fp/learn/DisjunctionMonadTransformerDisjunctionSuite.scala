package tech.backwards.fp.learn

import munit.ScalaCheckSuite
import org.scalacheck.Test

class DisjunctionMonadTransformerDisjunctionSuite extends ScalaCheckSuite {
  override protected def scalaCheckTestParameters: Test.Parameters =
    super.scalaCheckTestParameters.withMinSuccessfulTests(100).withMaxDiscardRatio(10)

  /**
   * String Disjunction (String Disjunction Int)
   */
  property("DisjunctionT") {
    val example: String Disjunction (String Disjunction Int) =
      Right(Right[String, Int](10))

    val transformer: DisjunctionT[Disjunction[String, *], String, Int] =
      DisjunctionT(Right[String, String Disjunction Int](Right[String, Int](10)))

    assertEquals(
      transformer.value,
      Right(Right[String, Int](10))
    )

    assertEquals(
      DisjunctionT(Right(Right[String, Int](10))).value,
      Right(Right[String, Int](10))
    )

    assertEquals(
      DisjunctionT(Right(Left[String, Int]("whoops"))).value,
      Right(Left[String, Int]("whoops"))
    )

    assertEquals(
      DisjunctionT(Left[String, String Disjunction Int]("whoops")).value,
      Left[String, String Disjunction Int]("whoops")
    )
  }

  property("DisjunctionT pure") {
    val transformer: DisjunctionT[Disjunction[String, *], String, Int] =
      DisjunctionT.pure[Disjunction[String, *], String, Int](10)

    assertEquals(
      transformer.value,
      Right(Right[String, Int](10))
    )
  }

  property("DisjunctionT lift") {
    val transformer: DisjunctionT[Disjunction[String, *], String, Int] =
      DisjunctionT.lift(Right(10))

    assertEquals(
      transformer.value,
      Right(Right[String, Int](10))
    )

    assertEquals(
      DisjunctionT.lift(Right(10)).value,
      Right(Right[String, Int](10))
    )

    assertEquals(
      DisjunctionT.lift(Left[String, Int]("whoops")).value,
      Left[String, String Disjunction Int]("whoops")
    )
  }

  property("DisjunctionT Functor") {
    val transformer: DisjunctionT[Disjunction[String, *], String, Int] =
      DisjunctionT(Right[String, String Disjunction Int](Right[String, Int](10)))

    assertEquals(
      Functor[DisjunctionT[Disjunction[String, *], String, *]].fmap(transformer)(_ + 1).value,
      Right(Right[String, Int](11))
    )

    assertEquals(
      Functor[DisjunctionT[Disjunction[String, *], String, *]].fmap(DisjunctionT(Right(Right[String, Int](10))))(_ + 1).value,
      Right(Right[String, Int](11))
    )

    assertEquals(
      Functor[DisjunctionT[Disjunction[String, *], String, *]].fmap(DisjunctionT(Right(Left[String, Int]("whoops"))))(_ + 1).value,
      Right(Left[String, Int]("whoops"))
    )

    assertEquals(
      Functor[DisjunctionT[Disjunction[String, *], String, *]].fmap(DisjunctionT(Left[String, String Disjunction Int]("whoops")))(_ + 1).value,
      Left[String, String Disjunction Int]("whoops")
    )
  }

  property("DisjunctionT Functor syntax") {
    import tech.backwards.fp.learn.Disjunction.syntax._
    import tech.backwards.fp.learn.Functor.syntax._

    val transformer: DisjunctionT[Disjunction[String, *], String, Int] =
      DisjunctionT(10.right.right)

    assertEquals(
      transformer.fmap(_ + 1).value,
      11.right.right
    )

    assertEquals(
      DisjunctionT(10.right[String].right).fmap(_ + 1).value,
      11.right.right
    )

    assertEquals(
      DisjunctionT(10.right[String].right) fmap (_ + 1),
      DisjunctionT(11.right[String].right)
    )

    assertEquals(
      DisjunctionT(10.right[String].right) `<$>` (_ + 1),
      DisjunctionT(11.right[String].right)
    )

    assertEquals(
      DisjunctionT("whoops".left[Int].right) `<$>` (_ + 1),
      DisjunctionT("whoops".left[Int].right)
    )

    assertEquals(
      DisjunctionT("whoops".left[String Disjunction Int]) `<$>` (_ + 1),
      DisjunctionT("whoops".left[String Disjunction Int])
    )
  }

  property("DisjunctionT Monad") {
    val transformer: DisjunctionT[Disjunction[String, *], String, Int] =
      Monad[DisjunctionT[Disjunction[String, *], String, *]].pure(10)

    assertEquals(
      Monad[DisjunctionT[Disjunction[String, *], String, *]].flatMap(transformer)(a => Monad[DisjunctionT[Disjunction[String, *], String, *]].pure(a + 1)).value,
      Right(Right(11))
    )

    assertEquals(
      Monad[DisjunctionT[Disjunction[String, *], String, *]].flatMap(Monad[DisjunctionT[Disjunction[String, *], String, *]].pure(10))(a => Monad[DisjunctionT[Disjunction[String, *], String, *]].pure(a + 1)).value,
      Right(Right(11))
    )

    assertEquals(
      Monad[DisjunctionT[Disjunction[String, *], String, *]].flatMap(DisjunctionT(Right(Left[String, Int]("whoops"))))(a => Monad[DisjunctionT[Disjunction[String, *], String, *]].pure(a + 1)).value,
      Right(Left[String, Int]("whoops"))
    )

    assertEquals(
      Monad[DisjunctionT[Disjunction[String, *], String, *]].flatMap(Monad[DisjunctionT[Disjunction[String, *], String, *]].pure(10))(_ => DisjunctionT(Right(Left[String, Int]("whoops")))).value,
      Right(Left[String, Int]("whoops"))
    )

    assertEquals(
      Monad[DisjunctionT[Disjunction[String, *], String, *]].flatMap(DisjunctionT(Right(Left[String, Int]("whoops"))))(_ => DisjunctionT(Right(Left[String, Int]("I did it again")))).value,
      Right(Left[String, Int]("whoops"))
    )

    assertEquals(
      Monad[DisjunctionT[Disjunction[String, *], String, *]].flatMap(DisjunctionT(Left[String, String Disjunction Int]("whoops")))(_ => DisjunctionT(Right(Right(10)))).value,
      Left[String, String Disjunction Int]("whoops")
    )

    assertEquals(
      Monad[DisjunctionT[Disjunction[String, *], String, *]].flatMap(DisjunctionT(Left[String, String Disjunction Int]("whoops")))(_ => DisjunctionT(Right(Left[String, Int]("whoops")))).value,
      Left[String, String Disjunction Int]("whoops")
    )

    assertEquals(
      Monad[DisjunctionT[Disjunction[String, *], String, *]].flatMap(DisjunctionT(Right(Left[String, Int]("whoops"))))(_ => DisjunctionT(Left[String, String Disjunction Int]("I did it again"))).value,
      Right(Left[String, Int]("whoops"))
    )
  }

  property("DisjunctionT Monad syntax") {
    import tech.backwards.fp.learn.Disjunction.syntax._
    import tech.backwards.fp.learn.Monad.syntax._

    val transformer: DisjunctionT[Disjunction[String, *], String, Int] =
      10.pure[DisjunctionT[Disjunction[String, *], String, *]]

    assertEquals(
      transformer.flatMap(a => (a + 1).pure[DisjunctionT[Disjunction[String, *], String, *]]).value,
      11.right.right
    )

    assertEquals(
      10.pure[DisjunctionT[Disjunction[String, *], String, *]].flatMap(a => (a + 1).pure[DisjunctionT[Disjunction[String, *], String, *]]).value,
      11.right.right
    )

    assertEquals(
      DisjunctionT("whoops".left[Int].right[String]).flatMap(a => (a + 1).pure[DisjunctionT[Disjunction[String, *], String, *]]).value,
      "whoops".left[Int].right
    )

    assertEquals(
      10.pure[DisjunctionT[Disjunction[String, *], String, *]].flatMap(_ => DisjunctionT("whoops".left[Int].right)).value,
      "whoops".left[Int].right
    )

    assertEquals(
      DisjunctionT("whoops".left[Int].right).flatMap(_ => DisjunctionT("I did it again".left[Int].right)).value,
      "whoops".left[Int].right
    )

    assertEquals(
      DisjunctionT("whoops".left[String Disjunction Int]).flatMap(_ => DisjunctionT("I did it again".left[Int].right)).value,
      "whoops".left[String Disjunction Int]
    )

    assertEquals(
      DisjunctionT("whoops".left[Int].right[String]).flatMap(_ => DisjunctionT("I did it again".left[String Disjunction Int])).value,
      "whoops".left[Int].right
    )
  }

  property("DisjunctionT Applicative") {
    val transformerFn: DisjunctionT[Disjunction[String, *], String, Int => Int] =
      Applicative[DisjunctionT[Disjunction[String, *], String, *]].pure(_ + 1)

    val transformer: DisjunctionT[Disjunction[String, *], String, Int] =
      Applicative[DisjunctionT[Disjunction[String, *], String, *]].pure(10)

    assertEquals(
      Applicative[DisjunctionT[Disjunction[String, *], String, *]].ap(transformerFn)(transformer).value,
      Right(Right[String, Int](11))
    )

    assertEquals(
      Applicative[DisjunctionT[Disjunction[String, *], String, *]].ap(Applicative[DisjunctionT[Disjunction[String, *], String, *]].pure((x: Int) => x + 1))(Applicative[DisjunctionT[Disjunction[String, *], String, *]].pure(10)).value,
      Right(Right[String, Int](11))
    )

    assertEquals(
      Applicative[DisjunctionT[Disjunction[String, *], String, *]].ap(DisjunctionT(Right(Left[String, Int => Int]("whoops"))))(Applicative[DisjunctionT[Disjunction[String, *], String, *]].pure(10)).value,
      Right(Left[String, Int]("whoops"))
    )

    // A Disjunction (X Disjunction Y)
    assertEquals(
      Applicative[DisjunctionT[Disjunction[String, *], String, *]].ap(DisjunctionT(Left[String, String Disjunction (Int => Int)]("whoops")))(Applicative[DisjunctionT[Disjunction[String, *], String, *]].pure(10)).value,
      Left[String, String Disjunction Int]("whoops")
    )

    assertEquals(
      Applicative[DisjunctionT[Disjunction[String, *], String, *]].ap(Applicative[DisjunctionT[Disjunction[String, *], String, *]].pure((x: Int) => x + 1))(DisjunctionT(Right(Left[String, Int]("whoops")))).value,
      Right(Left[String, Int]("whoops"))
    )

    assertEquals(
      Applicative[DisjunctionT[Disjunction[String, *], String, *]].ap(Applicative[DisjunctionT[Disjunction[String, *], String, *]].pure((x: Int) => x + 1))(DisjunctionT(Left[String, String Disjunction Int]("whoops"))).value,
      Left[String, String Disjunction Int]("whoops")
    )
  }

  property("DisjunctionT Applicative syntax") {
    import tech.backwards.fp.learn.Applicative.syntax._
    import tech.backwards.fp.learn.Disjunction.syntax._

    val transformerFn: DisjunctionT[Disjunction[String, *], String, Int => Int] =
      ((x: Int) => x + 1).pure[DisjunctionT[Disjunction[String, *], String, *]]

    val transformer: DisjunctionT[Disjunction[String, *], String, Int] =
      10.pure[DisjunctionT[Disjunction[String, *], String, *]]

    assertEquals(
      transformerFn.ap(transformer).value,
      11.right.right
    )

    assertEquals(
      ((x: Int) => x + 1).pure[DisjunctionT[Disjunction[String, *], String, *]].ap(10.pure[DisjunctionT[Disjunction[String, *], String, *]]).value,
      11.right.right
    )

    assertEquals(
      DisjunctionT("whoops".left[Int => Int].right[String]).ap(10.pure[DisjunctionT[Disjunction[String, *], String, *]]).value,
      "whoops".left[Int].right
    )

    assertEquals(
      DisjunctionT("whoops".left[String Disjunction (Int => Int)]).ap(10.pure[DisjunctionT[Disjunction[String, *], String, *]]).value,
      "whoops".left[String Disjunction Int]
    )

    assertEquals(
      ((x: Int) => x + 1).pure[DisjunctionT[Disjunction[String, *], String, *]].ap(DisjunctionT("whoops".left[Int].right)).value,
      "whoops".left[Int].right
    )

    assertEquals(
      ((x: Int) => x + 1).pure[DisjunctionT[Disjunction[String, *], String, *]].ap(DisjunctionT("whoops".left[String Disjunction Int])).value,
      "whoops".left[String Disjunction Int]
    )
  }

  property("DisjunctionT Functor and Applicative syntax") {
    import tech.backwards.fp.learn.Applicative.syntax._
    import tech.backwards.fp.learn.Disjunction.syntax._
    import tech.backwards.fp.learn.Functor.syntax._

    assertEquals(
      DisjunctionT(10.right[String].right) `<$>` (x => (y: Int) => x + y) <*> DisjunctionT(1.right.right),
      DisjunctionT(11.right[String].right)
    )

    assertEquals(
      DisjunctionT("whoops".left[Int].right) `<$>` (x => (y: Int) => x + y) <*> DisjunctionT(1.right.right),
      DisjunctionT("whoops".left[Int].right)
    )

    assertEquals(
      DisjunctionT("whoops".left[String Disjunction Int]) `<$>` (x => (y: Int) => x + y) <*> DisjunctionT(1.right.right),
      DisjunctionT("whoops".left[String Disjunction Int])
    )

    assertEquals(
      DisjunctionT(10.right[String].right) `<$>` (x => (y: Int) => x + y) <*> DisjunctionT("whoops".left[Int].right),
      DisjunctionT("whoops".left[Int].right)
    )

    // A Disjunction (X Disjunction Y)
    assertEquals(
      DisjunctionT(10.right[String].right[String]) `<$>` (x => (y: Int) => x + y) <*> DisjunctionT("whoops".left[String Disjunction Int]),
      DisjunctionT("whoops".left[String Disjunction Int])
    )

    val add: Int => Int => Int =
      x => y => x + y

    assertEquals(
      DisjunctionT(10.right[String].right[String]) `<$>` add <*> DisjunctionT("whoops".left[Int].right[String]),
      DisjunctionT("whoops".left[Int].right[String])
    )
  }

  property("DisjunctionT Functor and Applicative function syntax") {
    import tech.backwards.fp.learn.Applicative.syntax._
    import tech.backwards.fp.learn.Disjunction.syntax._
    import tech.backwards.fp.learn.Functor.syntax.function._

    assertEquals(
      ((x: Int) => (y: Int) => x + y) `<$>` DisjunctionT(10.right.right) <*> DisjunctionT(1.right.right),
      DisjunctionT(11.right.right)
    )

    assertEquals(
      ((x: Int) => (y: Int) => x + y) `<$>` DisjunctionT("whoops".left[Int].right) <*> DisjunctionT(1.right.right),
      DisjunctionT("whoops".left[Int].right)
    )

    assertEquals(
      ((x: Int) => (y: Int) => x + y) `<$>` DisjunctionT("whoops".left[String Disjunction Int]) <*> DisjunctionT(1.right.right),
      DisjunctionT("whoops".left[String Disjunction Int])
    )

    assertEquals(
      ((x: Int) => (y: Int) => x + y) `<$>` DisjunctionT(10.right[String].right) <*> DisjunctionT("whoops".left.right),
      DisjunctionT("whoops".left[Int].right)
    )

    assertEquals(
      ((x: Int) => (y: Int) => x + y) `<$>` DisjunctionT(10.right[String].right[String]) <*> DisjunctionT("whoops".left[String Disjunction Int]),
      DisjunctionT("whoops".left[String Disjunction Int])
    )

    val add: Int => Int => Int =
      x => y => x + y

    assertEquals(
      add `<$>` DisjunctionT(10.right.right) <*> DisjunctionT(1.right.right),
      DisjunctionT(11.right.right)
    )
  }

  property("DisjunctionT for comprehension") {
    import tech.backwards.fp.learn.Disjunction.syntax._
    import tech.backwards.fp.learn.Functor.syntax._
    import tech.backwards.fp.learn.Monad.syntax._

    val transformer: DisjunctionT[Disjunction[String, *], String, Int] =
      for {
        x <- 10.pure[DisjunctionT[Disjunction[String, *], String, *]]
        y <- 11.pure[DisjunctionT[Disjunction[String, *], String, *]]
        z <- 12.pure[DisjunctionT[Disjunction[String, *], String, *]]
      } yield x + y + z

    assertEquals(
      transformer.value,
      Right(Right(33))
    )

    assertEquals(
      for {
        x <- DisjunctionT.lift[Disjunction[String, *], String, Int](Right(10))
        y <- DisjunctionT.lift[Disjunction[String, *], String, Int](Right(11))
        z <- DisjunctionT.lift[Disjunction[String, *], String, Int](Right(12))
        _ <- DisjunctionT(0.right[String].map(Right.apply[String, Int])) // Without "lift"
      } yield x + y + z,
      DisjunctionT(Right[String, String Disjunction Int](33.right[String]))
    )

    assertEquals(
      for {
        x <- DisjunctionT.lift[Disjunction[String, *], String, Int](Right(10))
        y <- DisjunctionT("whoops".left[Int].right[String])
        z <- DisjunctionT.lift[Disjunction[String, *], String, Int](Right(12))
      } yield x + y + z,
      DisjunctionT("whoops".left[Int].right[String])
    )

    assertEquals(
      for {
        x <- DisjunctionT.lift[Disjunction[String, *], String, Int](Right(10))
        y <- DisjunctionT("whoops".left[String Disjunction Int])
        z <- DisjunctionT.lift[Disjunction[String, *], String, Int](Right(12))
      } yield x + y + z,
      DisjunctionT("whoops".left[String Disjunction Int])
    )
  }
}