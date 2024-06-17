package tech.backwards.fp.learn

import munit.ScalaCheckSuite
import org.scalacheck.Test

class DisjunctionMonadTransformerWriterSuite extends ScalaCheckSuite {
  override protected def scalaCheckTestParameters: Test.Parameters =
    super.scalaCheckTestParameters.withMinSuccessfulTests(100).withMaxDiscardRatio(10)

  property("DisjunctionT") {
    val transformer: DisjunctionT[Writer[String, *], String, Int] =
      DisjunctionT(Writer("" -> Right(10)))

    val example: Writer[String, String Disjunction Int] =
      transformer.value

    assertEquals(
      transformer.value.run(),
      "" -> Right(10)
    )

    assertEquals(
      DisjunctionT(Writer(Nil -> Left[String, Int]("whoops"))).value.run(),
      Nil -> Left("whoops")
    )
  }

  property("DisjunctionT pure") {
    val transformer: DisjunctionT[Writer[String, *], String, Int] =
      DisjunctionT.pure[Writer[String, *], String, Int](10)

    assertEquals(
      transformer.value.run(),
      "" -> Right(10)
    )

    assertEquals(
      DisjunctionT.pure[Writer[List[String], *], String, Int](10).value.run(),
      Nil -> Right(10)
    )
  }

  property("DisjunctionT lift") {
    val transformer: DisjunctionT[Writer[String, *], String, Int] =
      DisjunctionT.lift(Writer("" -> 10))

    assertEquals(
      transformer.value.run(),
      "" -> Right(10)
    )

    assertEquals(
      DisjunctionT.lift(Writer(Nil -> 10)).value.run(),
      Nil -> Right(10)
    )
  }

  property("DisjunctionT Functor") {
    val transformer: DisjunctionT[Writer[String, *], String, Int] =
      DisjunctionT(Writer("" -> Right(10)))

    assertEquals(
      Functor[DisjunctionT[Writer[String, *], String, *]].fmap(transformer)(_ + 1).value.run(),
      "" -> Right(11)
    )

    assertEquals(
      Functor[DisjunctionT[Writer[List[String], *], String, *]].fmap(DisjunctionT(Writer(Nil -> Right[String, Int](10))))(_ + 1).value.run(),
      Nil -> Right(11)
    )

    assertEquals(
      Functor[DisjunctionT[Writer[List[String], *], String, *]].fmap(DisjunctionT(Writer(Nil -> Left[String, Int]("whoops"))))(_ + 1).value.run(),
      Nil -> Left[String, Int]("whoops")
    )
  }

  property("DisjunctionT Functor syntax") {
    import tech.backwards.fp.learn.Disjunction.syntax._
    import tech.backwards.fp.learn.Functor.syntax._

    val transformer: DisjunctionT[Writer[String, *], String, Int] =
      DisjunctionT(Writer("" -> 10.right))

    assertEquals(
      transformer.fmap(_ + 1).value.run(),
      "" -> 11.right
    )

    assertEquals(
      DisjunctionT(Writer(Nil -> 10.right[String])).fmap(_ + 1).value.run(),
      Nil -> 11.right
    )

    assertEquals(
      DisjunctionT(Writer(Nil -> "whoops".left[Int])).fmap(_ + 1).value.run(),
      Nil -> "whoops".left[Int]
    )
  }

  property("DisjunctionT Monad") {
    val transformer: DisjunctionT[Writer[String, *], String, Int] =
      Monad[DisjunctionT[Writer[String, *], String, *]].pure(10)

    assertEquals(
      Monad[DisjunctionT[Writer[String, *], String, *]].flatMap(transformer)(a => Monad[DisjunctionT[Writer[String, *], String, *]].pure(a + 1)).value.run(),
      "" -> Right(11)
    )

    assertEquals(
      Monad[DisjunctionT[Writer[List[String], *], String, *]].flatMap(Monad[DisjunctionT[Writer[List[String], *], String, *]].pure(10))(a => Monad[DisjunctionT[Writer[List[String], *], String, *]].pure(a + 1)).value.run(),
      Nil -> Right(11)
    )

    assertEquals(
      Monad[DisjunctionT[Writer[List[String], *], String, *]].flatMap(DisjunctionT(Writer(Nil -> Left[String, Int]("whoops"))))(a => Monad[DisjunctionT[Writer[List[String], *], String, *]].pure(a + 1)).value.run(),
      Nil -> Left[String, Int]("whoops")
    )

    assertEquals(
      Monad[DisjunctionT[Writer[List[String], *], String, *]].flatMap(Monad[DisjunctionT[Writer[List[String], *], String, *]].pure(10))(_ => DisjunctionT(Writer(Nil -> Left[String, Int]("whoops")))).value.run(),
      Nil -> Left[String, Int]("whoops")
    )

    assertEquals(
      Monad[DisjunctionT[Writer[List[String], *], String, *]].flatMap(DisjunctionT(Writer(Nil -> Right[String, Int](1))))(a => DisjunctionT(Writer(Nil -> Right[String, Int](a + 1)))).value.run(),
      Nil -> Right(2)
    )

    assertEquals(
      Monad[DisjunctionT[Writer[List[String], *], String, *]].flatMap(DisjunctionT(Writer(List("foo") -> Right[String, Int](1))))(a => DisjunctionT(Writer(List("bar") -> Right[String, Int](a + 1)))).value.run(),
      List("foo", "bar") -> Right(2)
    )
  }

  property("DisjunctionT Monad syntax") {
    import tech.backwards.fp.learn.Disjunction.syntax._
    import tech.backwards.fp.learn.Monad.syntax._

    val transformer: DisjunctionT[Writer[String, *], String, Int] =
      10.pure[DisjunctionT[Writer[String, *], String, *]]

    assertEquals(
      transformer.flatMap(a => (a + 1).pure[DisjunctionT[Writer[String, *], String, *]]).value.run(),
      "" -> 11.right
    )

    assertEquals(
      10.pure[DisjunctionT[Writer[List[String], *], String, *]].flatMap(a => (a + 1).pure[DisjunctionT[Writer[List[String], *], String, *]]).value.run(),
      Nil -> 11.right
    )

    assertEquals(
      DisjunctionT(Writer(List.empty[String] -> "whoops".left[Int])).flatMap(a => (a + 1).pure[DisjunctionT[Writer[List[String], *], String, *]]).value.run(),
      Nil -> "whoops".left[Int]
    )

    assertEquals(
      10.pure[DisjunctionT[Writer[List[String], *], String, *]].flatMap(_ => DisjunctionT(Writer(Nil -> "whoops".left[Int]))).value.run(),
      Nil -> "whoops".left[Int]
    )

    assertEquals(
      DisjunctionT(Writer(List.empty[String] -> 1.right[String])).flatMap(a => DisjunctionT(Writer(Nil -> (a + 1).right[String]))).value.run(),
      Nil -> 2.right
    )

    assertEquals(
      DisjunctionT(Writer(List("foo") -> 1.right[String])).flatMap(a => DisjunctionT(Writer(List("bar") -> (a + 1).right[String]))).value.run(),
      List("foo", "bar") -> 2.right
    )
  }

  property("DisjunctionT Applicative") {
    val transformerFn: DisjunctionT[Writer[String, *], String, Int => Int] =
      Applicative[DisjunctionT[Writer[String, *], String, *]].pure(_ + 1)

    val transformer: DisjunctionT[Writer[String, *], String, Int] =
      Applicative[DisjunctionT[Writer[String, *], String, *]].pure(10)

    assertEquals(
      Applicative[DisjunctionT[Writer[String, *], String, *]].ap(transformerFn)(transformer).value.run(),
      "" -> Right(11)
    )

    assertEquals(
      Applicative[DisjunctionT[Writer[List[String], *], String, *]].ap(Applicative[DisjunctionT[Writer[List[String], *], String, *]].pure((x: Int) => x + 1))(Applicative[DisjunctionT[Writer[List[String], *], String, *]].pure(10)).value.run(),
      Nil -> Right(11)
    )

    assertEquals(
      Applicative[DisjunctionT[Writer[List[String], *], String, *]].ap(DisjunctionT(Writer(Nil -> Left[String, Int => Int]("whoops"))))(Applicative[DisjunctionT[Writer[List[String], *], String, *]].pure(10)).value.run(),
      Nil -> Left[String, Int]("whoops")
    )

    assertEquals(
      Applicative[DisjunctionT[Writer[List[String], *], String, *]].ap(Applicative[DisjunctionT[Writer[List[String], *], String, *]].pure((x: Int) => x + 1))(DisjunctionT(Writer(Nil -> Left[String, Int]("whoops")))).value.run(),
      Nil -> Left[String, Int]("whoops")
    )

    assertEquals(
      Applicative[DisjunctionT[Writer[List[String], *], String, *]].ap(DisjunctionT(Writer(Nil -> Right[String, Int => Int]((x: Int) => x + 1))))(DisjunctionT(Writer(Nil -> Left[String, Int]("whoops")))).value.run(),
      Nil -> Left[String, Int]("whoops")
    )

    assertEquals(
      Applicative[DisjunctionT[Writer[List[String], *], String, *]].ap(DisjunctionT(Writer(Nil -> Right[String, Int => Int]((x: Int) => x + 1))))(DisjunctionT(Writer(Nil -> Right(1)))).value.run(),
      Nil -> Right(2)
    )

    assertEquals(
      Applicative[DisjunctionT[Writer[List[String], *], String, *]].ap(DisjunctionT(Writer(List("foo") -> Right[String, Int => Int]((x: Int) => x + 1))))(DisjunctionT(Writer(List("bar") -> Right(1)))).value.run(),
      List("foo", "bar") -> Right(2)
    )
  }

  property("DisjunctionT Applicative syntax") {
    import tech.backwards.fp.learn.Applicative.syntax._
    import tech.backwards.fp.learn.Disjunction.syntax._

    val transformerFn: DisjunctionT[Writer[String, *], String, Int => Int] =
      ((x: Int) => x + 1).pure[DisjunctionT[Writer[String, *], String, *]]

    val transformer: DisjunctionT[Writer[String, *], String, Int] =
      10.pure[DisjunctionT[Writer[String, *], String, *]]

    assertEquals(
      transformerFn.ap(transformer).value.run(),
      "" -> 11.right
    )

    assertEquals(
      ((x: Int) => x + 1).pure[DisjunctionT[Writer[List[String], *], String, *]].ap(10.pure[DisjunctionT[Writer[List[String], *], String, *]]).value.run(),
      Nil -> 11.right
    )

    assertEquals(
      DisjunctionT(Writer(List.empty[String] -> "whoops".left[Int => Int])).ap(10.pure[DisjunctionT[Writer[List[String], *], String, *]]).value.run(),
      Nil -> "whoops".left[Int]
    )

    assertEquals(
      ((x: Int) => x + 1).pure[DisjunctionT[Writer[List[String], *], String, *]].ap(DisjunctionT(Writer(Nil -> "whoops".left[Int]))).value.run(),
      Nil -> "whoops".left[Int]
    )

    assertEquals(
      DisjunctionT(Writer(List.empty[String] -> ((x: Int) => x + 1).right[String])).ap(DisjunctionT(Writer(Nil -> 1.right))).value.run(),
      Nil -> 2.right
    )

    assertEquals(
      DisjunctionT(Writer(List("foo") -> ((x: Int) => x + 1).right[String])).ap(DisjunctionT(Writer(List("bar") -> 1.right))).value.run(),
      List("foo", "bar") -> 2.right
    )
  }

  property("DisjunctionT Functor and Applicative syntax") {
    import tech.backwards.fp.learn.Applicative.syntax._
    import tech.backwards.fp.learn.Disjunction.syntax._
    import tech.backwards.fp.learn.Functor.syntax._

    assertEquals(
      (DisjunctionT(Writer("foo" -> 10.right[String])) `<$>` (x => (y: Int) => x + y) <*> DisjunctionT(Writer("bar" -> 1.right))).value.run(),
      "foobar" -> 11.right
    )

    assertEquals(
      (DisjunctionT(Writer(List("foo") -> "whoops".left[Int])) `<$>` (x => (y: Int) => x + y) <*> DisjunctionT(Writer(List("bar") -> 1.right))).value.run(),
      List("foo", "bar") -> "whoops".left[Int]
    )

    assertEquals(
      (DisjunctionT(Writer(List("foo") -> 1.right[String])) `<$>` (x => (y: Int) => x + y) <*> DisjunctionT(Writer(List("bar") -> "whoops".left[Int]))).value.run(),
      List("foo", "bar") -> "whoops".left[Int]
    )

    val add: Int => Int => Int =
      x => y => x + y

    assertEquals(
      (DisjunctionT(Writer(List("foo") -> 10.right[String])) `<$>` add <*> DisjunctionT(Writer(List("bar") -> "whoops".left[Int]))).value.run(),
      List("foo", "bar") -> "whoops".left[Int]
    )

    assertEquals(
      (DisjunctionT(Writer(List("foo") -> 10.right[String])) `<$>` add <*> DisjunctionT(Writer(List("bar") -> 1.right))).value.run(),
      List("foo", "bar") -> 11.right
    )
  }

  property("DisjunctionT Functor and Applicative function syntax") {
    import tech.backwards.fp.learn.Applicative.syntax._
    import tech.backwards.fp.learn.Disjunction.syntax._
    import tech.backwards.fp.learn.Functor.syntax.function._

    assertEquals(
      (((x: Int) => (y: Int) => x + y) `<$>` DisjunctionT(Writer("foo" -> 10.right[String])) <*> DisjunctionT(Writer("bar" -> 1.right))).value.run(),
      "foobar" -> 11.right
    )

    assertEquals(
      (((x: Int) => (y: Int) => x + y) `<$>` DisjunctionT(Writer(List("foo") -> "whoops".left[Int])) <*> DisjunctionT(Writer(List("bar") -> 1.right))).value.run(),
      List("foo", "bar") -> "whoops".left[Int]
    )

    assertEquals(
      (((x: Int) => (y: Int) => x + y) `<$>` DisjunctionT(Writer(List("foo") -> 10.right[String])) <*> DisjunctionT(Writer(List("bar") -> "whoops".left[Int]))).value.run(),
      List("foo", "bar") -> "whoops".left[Int]
    )

    val add: Int => Int => Int =
      x => y => x + y

    assertEquals(
      (add `<$>` DisjunctionT(Writer(List("foo") -> 10.right[String])) <*> DisjunctionT(Writer(List("bar") -> 1.right))).value.run(),
      List("foo", "bar") -> 11.right
    )

    assertEquals(
      (add `<$>` DisjunctionT(Writer(List("foo") -> 10.right[String])) <*> DisjunctionT(Writer(List("bar") -> "whoops".left[Int]))).value.run(),
      List("foo", "bar") -> "whoops".left[Int]
    )
  }

  property("DisjunctionT for comprehension") {
    import tech.backwards.fp.learn.Disjunction.syntax._
    import tech.backwards.fp.learn.Functor.syntax._
    import tech.backwards.fp.learn.Monad.syntax._

    val transformer: DisjunctionT[Writer[String, *], String, Int] =
      for {
        x <- 10.pure[DisjunctionT[Writer[String, *], String, *]]
        y <- 11.pure[DisjunctionT[Writer[String, *], String, *]]
        z <- 12.pure[DisjunctionT[Writer[String, *], String, *]]
      } yield x + y + z

    assertEquals(
      transformer.value.run(),
      "" -> 33.right
    )

    assertEquals(
      (
        for {
          x <- DisjunctionT.lift[Writer[List[String], *], String, Int](Writer(List("foo") -> 10))
          y <- DisjunctionT.lift[Writer[List[String], *], String, Int](Writer(List("bar") -> 11))
          z <- DisjunctionT.lift[Writer[List[String], *], String, Int](Writer(List("baz") -> 12))
          _ <- DisjunctionT(Writer(List("blah") -> 0).map(Right[String, Int]))
        } yield x + y + z
      ).value.run(),
      List("foo", "bar", "baz", "blah") -> 33.right
    )

    assertEquals(
      (
        for {
          x <- DisjunctionT(Writer(List("foo") -> 10.right[String]))
          y <- DisjunctionT(Writer(List("bar") -> 11.right[String]))
          z <- DisjunctionT(Writer(List("baz") -> 12.right[String]))
          _ <- DisjunctionT(Writer(List("blah") -> "whoops".left[Int]))
        } yield x + y + z
      ).value.run(),
      List("foo", "bar", "baz", "blah") -> "whoops".left
    )

    assertEquals(
      (
        for {
          x <- DisjunctionT(Writer(List("foo") -> "whoops".left[Int]))
          y <- DisjunctionT(Writer(List("bar") -> 11.right[String]))
          z <- DisjunctionT(Writer(List("baz") -> 12.right[String]))
          _ <- DisjunctionT(Writer(List("blah") -> 0).map(Right[String, Int]))
        } yield x + y + z
      ).value.run(),
      List("foo") -> "whoops".left
    )
  }
}