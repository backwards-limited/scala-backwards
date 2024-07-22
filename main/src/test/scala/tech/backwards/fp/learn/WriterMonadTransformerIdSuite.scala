package tech.backwards.fp.learn

import munit.ScalaCheckSuite
import org.scalacheck.Test

/**
 * First of the following WriterT suites:
 *  - WriterMonadTransformerMaybeSuite
 *  - WriterMonadTransformerDisjunctionSuite
 *  - WriterMonadTransformerListSuite
 *  - WriterMonadTransformerWriterSuite
 *  - WriterMonadTransformerStateSuite
 */
class WriterMonadTransformerIdSuite extends ScalaCheckSuite {
  override protected def scalaCheckTestParameters: Test.Parameters =
    super.scalaCheckTestParameters.withMinSuccessfulTests(100).withMaxDiscardRatio(10)

  property("WriterT") {
    val transformer: WriterT[Id, List[String], Int] =
      WriterT(() => Id(List("foo") -> 10))

    assertEquals(
      transformer.run().value,
      List("foo") -> 10
    )

    assertEquals(
      WriterT(() => Id(List("foo") -> 10)).run().value,
      List("foo") -> 10
    )
  }

  property("WriterT pure") {
    val transformer: WriterT[Id, List[String], Int] =
      WriterT.pure[Id, List[String], Int](10)

    assertEquals(
      transformer.run().value,
      Nil -> 10
    )
  }

  property("WriterT tell") {
    val transformer: WriterT[Id, List[String], Unit] =
      WriterT.tell(List("foo", "bar"))

    assertEquals(
      transformer.run().value,
      List("foo", "bar") -> ()
    )
  }

  property("WriterT lift") {
    val transformer: WriterT[Id, List[String], Int] =
      WriterT.lift(Id(10))

    assertEquals(
      transformer.run().value,
      Nil -> 10
    )
  }

  property("WriterT Functor") {
    val transformer: WriterT[Id, List[String], Int] =
      WriterT(() => Id(List("foo") -> 10))

    assertEquals(
      Functor[WriterT[Id, List[String], *]].fmap(transformer)(_ + 1).run().value,
      List("foo") -> 11
    )
  }

  property("WriterT Functor syntax") {
    import tech.backwards.fp.learn.Functor.syntax._

    val transformer: WriterT[Id, List[String], Int] =
      WriterT(() => Id(List("foo") -> 10))

    assertEquals(
      transformer.fmap(_ + 1).run().value,
      List("foo") -> 11
    )

    assertEquals(
      WriterT(() => Id(List("foo") -> 10)).fmap(_ + 1).run().value,
      List("foo") -> 11
    )

    assertEquals(
      (WriterT(() => Id(List("foo") -> 10)) `<$>` (_ + 1)).run().value,
      List("foo") -> 11
    )
  }

  property("WriterT Monad") {
    val transformer: WriterT[Id, List[String], Int] =
      Monad[WriterT[Id, List[String], *]].pure(10)

    assertEquals(
      transformer.run().value,
      Nil -> 10
    )

    assertEquals(
      Monad[WriterT[Id, List[String], *]].flatMap(transformer)(a => Monad[WriterT[Id, List[String], *]].pure(a + 1)).run().value,
      Nil -> 11
    )

    assertEquals(
      Monad[WriterT[Id, List[String], *]].flatMap(Monad[WriterT[Id, List[String], *]].pure(10))(a => Monad[WriterT[Id, List[String], *]].pure(a + 1)).run().value,
      Nil -> 11
    )

    assertEquals(
      Monad[WriterT[Id, List[String], *]].flatMap(WriterT(() => Id(List("foo") -> 10)))(a => Monad[WriterT[Id, List[String], *]].pure(a + 1)).run().value,
      List("foo") -> 11
    )

    assertEquals(
      Monad[WriterT[Id, List[String], *]].flatMap(WriterT tell List("foo", "bar"))(_ => WriterT tell List("baz")).run().value,
      List("foo", "bar", "baz") -> ()
    )
  }

  property("WriterT Monad syntax") {
    import tech.backwards.fp.learn.Monad.syntax._

    val transformer: WriterT[Id, List[String], Int] =
      10.pure[WriterT[Id, List[String], *]]

    assertEquals(
      transformer.flatMap(a => (a + 1).pure[WriterT[Id, List[String], *]]).run().value,
      Nil -> 11
    )

    assertEquals(
      10.pure[WriterT[Id, List[String], *]].flatMap(a => (a + 1).pure[WriterT[Id, List[String], *]]).run().value,
      Nil -> 11
    )

    assertEquals(
      WriterT.tell[Id, List[String]](List("foo", "bar")).flatMap(_ => WriterT tell List("baz")).run().value,
      List("foo", "bar", "baz") -> ()
    )
  }

  property("WriterT Applicative") {
    val transformerFn: WriterT[Id, List[String], Int => Int] =
      Applicative[WriterT[Id, List[String], *]].pure(_ + 1)

    val transformer: WriterT[Id, List[String], Int] =
      Applicative[WriterT[Id, List[String], *]].pure(10)

    assertEquals(
      Applicative[WriterT[Id, List[String], *]].ap(transformerFn)(transformer).run().value,
      Nil -> 11
    )

    assertEquals(
      Applicative[WriterT[Id, List[String], *]].ap(Applicative[WriterT[Id, List[String], *]].pure((x: Int) => x + 1))(Applicative[WriterT[Id, List[String], *]].pure(10)).run().value,
      Nil -> 11
    )

    assertEquals(
      Applicative[WriterT[Id, List[String], *]].ap(Applicative[WriterT[Id, List[String], *]].pure((x: Int) => x + 1))(WriterT(() => Id(List("foo") -> 10))).run().value,
      List("foo") -> 11
    )
  }

  property("WriterT Applicative syntax") {
    import tech.backwards.fp.learn.Applicative.syntax._

    val transformerFn: WriterT[Id, List[String], Int => Int] =
      ((_: Int) + 1).pure[WriterT[Id, List[String], *]]

    val transformer: WriterT[Id, List[String], Int] =
      10.pure[WriterT[Id, List[String], *]]

    assertEquals(
      transformerFn.ap(transformer).run().value,
      Nil -> 11
    )

    assertEquals(
      ((_: Int) + 1).pure[WriterT[Id, List[String], *]].ap(10.pure[WriterT[Id, List[String], *]]).run().value,
      Nil -> 11
    )

    assertEquals(
      ((_: Int) + 1).pure[WriterT[Id, List[String], *]].ap(WriterT(() => Id(List("foo") -> 10))).run().value,
      List("foo") -> 11
    )

    assertEquals(
      (((_: Int) + 1).pure[WriterT[Id, List[String], *]] <*> WriterT(() => Id(List("foo") -> 10))).run().value,
      List("foo") -> 11
    )
  }

  property("WriterT Functor and Applicative syntax") {
    import tech.backwards.fp.learn.Applicative.syntax._
    import tech.backwards.fp.learn.Functor.syntax._

    assertEquals(
      (WriterT(() => Id(List("foo") -> 10)) `<$>` (x => (_: Int) + x) <*> WriterT(() => Id(List("bar") -> 1))).run().value,
      List("foo", "bar") -> 11
    )

    val add: Int => Int => Int =
      x => y => x + y

    assertEquals(
      (WriterT(() => Id(List("foo") -> 10)) `<$>` add <*> WriterT(() => Id(List("bar") -> 1))).run().value,
      List("foo", "bar") -> 11
    )
  }

  property("WriterT Functor and Applicative function syntax") {
    import tech.backwards.fp.learn.Applicative.syntax._
    import tech.backwards.fp.learn.Functor.syntax.function._

    assertEquals(
      (((x: Int) => (_: Int) + x) `<$>` WriterT(() => Id(List("foo") -> 10)) <*> WriterT(() => Id(List("bar") -> 1))).run().value,
      List("foo", "bar") -> 11
    )

    val add: Int => Int => Int =
      x => y => x + y

    assertEquals(
      (add `<$>` WriterT(() => Id(List("foo") -> 10)) <*> WriterT(() => Id(List("bar") -> 1))).run().value,
      List("foo", "bar") -> 11
    )

    assertEquals(
      (add `<$>` WriterT(() => Id(List("foo") -> 10)) <*> WriterT.pure(1)).run().value,
      List("foo") -> 11
    )
  }

  property("WriterT for comprehension") {
    import tech.backwards.fp.learn.Functor.syntax._
    import tech.backwards.fp.learn.Monad.syntax._

    val transformer1: WriterT[Id, List[String], Int] =
      for {
        x <- 10.pure[WriterT[Id, List[String], *]]
        y <- 11.pure[WriterT[Id, List[String], *]]
        z <- 12.pure[WriterT[Id, List[String], *]]
      } yield x + y + z

    assertEquals(
      transformer1.run().value,
      Nil -> 33
    )

    val transformer2: WriterT[Id, List[String], Int] =
      for {
        x <- 10.pure[WriterT[Id, List[String], *]]
        _ <- WriterT.tell[Id, List[String]](List("foo"))
        y <- 11.pure[WriterT[Id, List[String], *]]
        _ <- WriterT.tell[Id, List[String]](List("bar"))
        z <- 12.pure[WriterT[Id, List[String], *]]
      } yield x + y + z

    assertEquals(
      transformer2.run().value,
      List("foo", "bar") -> 33
    )
  }
}