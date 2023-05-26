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

  property("IdT Functor") {
    val transformer: IdT[Disjunction[String, *], Int] =
      IdT(Right(Id(10)))

    assertEquals(
      Functor[IdT[Disjunction[String, *], *]].fmap(transformer)(_ + 1).value,
      Right(Id(11))
    )

    assertEquals(
      Functor[IdT[Disjunction[String, *], *]].fmap(IdT(Right(Id(10))))(_ + 1).value,
      Right(Id(11))
    )

    assertEquals(
      Functor[IdT[Disjunction[String, *], *]].fmap(IdT(Left[String, Id[Int]]("whoops")))(_ + 1).value,
      Left("whoops")
    )
  }

  property("IdT Functor syntax") {
    import tech.backwards.fp.learn.Disjunction.syntax._
    import tech.backwards.fp.learn.Functor.syntax._

    val transformer: IdT[Disjunction[String, *], Int] =
      IdT(Id(10).right)

    assertEquals(
      transformer.fmap(_ + 1).value,
      Id(11).right
    )

    assertEquals(
      IdT(Id(10).right).fmap(_ + 1).value,
      Id(11).right
    )

    assertEquals(
      IdT(Id(10).right) fmap (_ + 1),
      IdT(Id(11).right)
    )

    assertEquals(
      IdT(Id(10).right) `<$>` (_ + 1),
      IdT(Id(11).right)
    )

    assertEquals(
      IdT("whoops".left[Id[Int]]) `<$>` (_ + 1),
      IdT("whoops".left[Id[Int]])
    )
  }

  property("IdT Monad") {
    val transformer: IdT[Disjunction[String, *], Int] =
      Monad[IdT[Disjunction[String, *], *]].pure(10)

    assertEquals(
      Monad[IdT[Disjunction[String, *], *]].flatMap(transformer)(a => Monad[IdT[Disjunction[String, *], *]].pure(a + 1)).value,
      Right(Id(11))
    )

    assertEquals(
      Monad[IdT[Disjunction[String, *], *]].flatMap(Monad[IdT[Disjunction[String, *], *]].pure(10))(a => Monad[IdT[Disjunction[String, *], *]].pure(a + 1)).value,
      Right(Id(11))
    )

    assertEquals(
      Monad[IdT[Disjunction[String, *], *]].flatMap(IdT(Left[String, Id[Int]]("whoops")))(a => Monad[IdT[Disjunction[String, *], *]].pure(a + 1)).value,
      Left("whoops")
    )

    assertEquals(
      Monad[IdT[Disjunction[String, *], *]].flatMap(Monad[IdT[Disjunction[String, *], *]].pure(10))(_ => IdT(Left[String, Id[Int]]("whoops"))).value,
      Left("whoops")
    )
  }

  property("IdT Monad syntax") {
    import tech.backwards.fp.learn.Disjunction.syntax._
    import tech.backwards.fp.learn.Monad.syntax._

    val transformer: IdT[Disjunction[String, *], Int] =
      10.pure[IdT[Disjunction[String, *], *]]

    assertEquals(
      transformer.flatMap(a => (a + 1).pure[IdT[Disjunction[String, *], *]]).value,
      Id(11).right
    )

    assertEquals(
      10.pure[IdT[Disjunction[String, *], *]].flatMap(a => (a + 1).pure[IdT[Disjunction[String, *], *]]).value,
      Id(11).right
    )

    assertEquals(
      10.pure[IdT[Disjunction[String, *], *]] flatMap (a => IdT(Id(a + 1).right)),
      IdT(Id(11).right[String])
    )

    assertEquals(
      10.pure[IdT[Disjunction[String, *], *]] >>= (a => IdT(Id(a + 1).right)),
      IdT(Id(11).right[String])
    )

    assertEquals(
      IdT("whoops".left[Id[Int]]) >>= (a => IdT(Id(a + 1).right)),
      IdT("whoops".left[Id[Int]])
    )

    assertEquals(
      IdT(Id(10).right[String]) >>= (_ => IdT("whoops".left[Id[Int]])),
      IdT("whoops".left[Id[Int]])
    )
  }

  property("IdT Applicative") {
    val transformerFn: IdT[Disjunction[String, *], Int => Int] =
      Applicative[IdT[Disjunction[String, *], *]].pure(_ + 1)

    val transformer: IdT[Disjunction[String, *], Int] =
      Applicative[IdT[Disjunction[String, *], *]].pure(10)

    assertEquals(
      Applicative[IdT[Disjunction[String, *], *]].ap(transformerFn)(transformer).value,
      Right(Id(11))
    )

    assertEquals(
      Applicative[IdT[Disjunction[String, *], *]].ap(Applicative[IdT[Disjunction[String, *], *]].pure((x: Int) => x + 1))(Applicative[IdT[Disjunction[String, *], *]].pure(10)).value,
      Right(Id(11))
    )

    assertEquals(
      Applicative[IdT[Disjunction[String, *], *]].ap(IdT(Left[String, Id[Int => Int]]("whoops")))(Applicative[IdT[Disjunction[String, *], *]].pure(10)).value,
      Left("whoops")
    )

    assertEquals(
      Applicative[IdT[Disjunction[String, *], *]].ap(Applicative[IdT[Disjunction[String, *], *]].pure((x: Int) => x + 1))(IdT(Left[String, Id[Int]]("whoops"))).value,
      Left("whoops")
    )
  }

  property("IdT Applicative syntax") {
    import tech.backwards.fp.learn.Applicative.syntax._
    import tech.backwards.fp.learn.Disjunction.syntax._

    val transformerFn: IdT[Disjunction[String, *], Int => Int] =
      ((x: Int) => x + 1).pure[IdT[Disjunction[String, *], *]]

    val transformer: IdT[Disjunction[String, *], Int] =
      10.pure[IdT[Disjunction[String, *], *]]

    assertEquals(
      transformerFn.ap(transformer).value,
      Id(11).right
    )

    assertEquals(
      ((x: Int) => x + 1).pure[IdT[Disjunction[String, *], *]].ap(10.pure[IdT[Disjunction[String, *], *]]).value,
      Id(11).right
    )

    assertEquals(
      ((x: Int) => x + 1).pure[IdT[Disjunction[String, *], *]].ap(IdT("whoops".left[Id[Int]])).value,
      "whoops".left
    )

    assertEquals(
      IdT("whoops".left[Id[Int => Int]]).ap(10.pure[IdT[Disjunction[String, *], *]]).value,
      "whoops".left
    )

    assertEquals(
      IdT("whoops".left[Id[Int => Int]]) ap 10.pure[IdT[Disjunction[String, *], *]],
      IdT("whoops".left[Id[Int]])
    )

    assertEquals(
      IdT("whoops".left[Id[Int => Int]]) <*> 10.pure[IdT[Disjunction[String, *], *]],
      IdT("whoops".left[Id[Int]])
    )

    assertEquals(
      10.pure[IdT[Disjunction[String, *], *]].ap(((x: Int) => x + 1).pure[IdT[Disjunction[String, *], *]]).value,
      Id(11).right
    )

    assertEquals(
      10.pure[IdT[Disjunction[String, *], *]].ap(IdT("whoops".left[Id[Int => Int]])).value,
      "whoops".left
    )

    assertEquals(
      IdT("whoops".left[Id[Int]]).ap(((x: Int) => x + 1).pure[IdT[Disjunction[String, *], *]]).value,
      "whoops".left
    )

    assertEquals(
      IdT("whoops".left[Id[Int]]) ap ((x: Int) => x + 1).pure[IdT[Disjunction[String, *], *]],
      IdT("whoops".left[Id[Int]])
    )

    assertEquals(
      IdT("whoops".left[Id[Int]]) <*> ((x: Int) => x + 1).pure[IdT[Disjunction[String, *], *]],
      IdT("whoops".left[Id[Int]])
    )
  }

  property("IdT Functor and Applicative syntax") {
    import tech.backwards.fp.learn.Applicative.syntax._
    import tech.backwards.fp.learn.Disjunction.syntax._
    import tech.backwards.fp.learn.Functor.syntax._

    assertEquals(
      IdT(Right(Id(10))) `<$>` (x => (y: Int) => x + y) <*> IdT(Right(Id(1))),
      IdT(Right(Id(11)))
    )

    assertEquals(
      IdT("whoops".left[Id[Int]]) `<$>` (x => (y: Int) => x + y) <*> IdT(Id(1).right),
      IdT("whoops".left[Id[Int]])
    )

    assertEquals(
      IdT(Id(10).right[String]) `<$>` (x => (y: Int) => x + y) <*> IdT("whoops".left[Id[Int]]),
      IdT("whoops".left[Id[Int]])
    )

    val add: Int => Int => Int =
      x => y => x + y

    assertEquals(
      IdT(Id(10).right[String]) `<$>` add <*> IdT("whoops".left[Id[Int]]),
      IdT("whoops".left[Id[Int]])
    )
  }

  property("IdT Functor and Applicative function syntax") {
    import tech.backwards.fp.learn.Applicative.syntax._
    import tech.backwards.fp.learn.Disjunction.syntax._
    import tech.backwards.fp.learn.Functor.syntax.function._

    assertEquals(
      ((x: Int) => (y: Int) => x + y) `<$>` IdT(Right(Id(10))) <*> IdT(Right(Id(1))),
      IdT(Right(Id(11)))
    )

    assertEquals(
      ((x: Int) => (y: Int) => x + y) `<$>` IdT("whoops".left[Id[Int]]) <*> IdT(Right(Id(1))),
      IdT("whoops".left[Id[Int]])
    )

    assertEquals(
      ((x: Int) => (y: Int) => x + y) `<$>` IdT(Id(10).right[String]) <*> IdT("whoops".left[Id[Int]]),
      IdT("whoops".left[Id[Int]])
    )

    val add: Int => Int => Int =
      x => y => x + y

    assertEquals(
      add `<$>` IdT(Right(Id(10))) <*> IdT(Right(Id(1))),
      IdT(Right(Id(11)))
    )
  }

  property("IdT for comprehension") {
    import tech.backwards.fp.learn.Disjunction.syntax._
    import tech.backwards.fp.learn.Functor.syntax._
    import tech.backwards.fp.learn.Monad.syntax._

    val transformer: IdT[String Disjunction *, Int] =
      for {
        x <- 10.pure[IdT[String Disjunction *, *]]
        y <- 11.pure[IdT[String Disjunction *, *]]
        z <- 12.pure[IdT[String Disjunction *, *]]
      } yield x + y + z

    assertEquals(
      transformer.value,
      Right(Id(33))
    )

    assertEquals(
      for {
        x <- IdT.lift(10.right[String])
        y <- IdT.lift(11.right[String])
        z <- IdT.lift(12.right[String])
        _ <- IdT(0.right[String].map(Id.apply)) // Without "lift"
      } yield x + y + z,
      IdT(Id(33).right[String])
    )

    assertEquals(
      for {
        x <- IdT.lift(10.right[String])
        y <- IdT.lift("whoops".left[Int])
        z <- IdT.lift(12.right[String])
      } yield x + y + z,
      IdT("whoops".left[Id[Int]])
    )
  }
}