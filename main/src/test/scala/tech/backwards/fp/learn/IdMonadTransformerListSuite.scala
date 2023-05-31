package tech.backwards.fp.learn

import munit.ScalaCheckSuite
import org.scalacheck.Test

class IdMonadTransformerListSuite extends ScalaCheckSuite {
  override protected def scalaCheckTestParameters: Test.Parameters =
    super.scalaCheckTestParameters.withMinSuccessfulTests(100).withMaxDiscardRatio(10)

  property("IdT") {
    val transformer: IdT[List, Int] =
      IdT(List(Id(10)))

    assertEquals(
      transformer.value,
      List(Id(10))
    )
  }

  property("IdT pure") {
    val transformer: IdT[List, Int] =
      IdT.pure[List, Int](10)

    assertEquals(
      transformer.value,
      List(Id(10))
    )
  }

  property("IdT lift") {
    val transformer: IdT[List, Int] =
      IdT.lift(List(10))

    assertEquals(
      transformer.value,
      List(Id(10))
    )

    assertEquals(
      IdT.lift(List(10)).value,
      List(Id(10))
    )

    assertEquals(
      IdT.lift(Nil).value,
      Nil
    )
  }

  property("IdT Functor") {
    val transformer: IdT[List, Int] =
      IdT(List(Id(10)))

    assertEquals(
      Functor[IdT[List, *]].fmap(transformer)(_ + 1).value,
      List(Id(11))
    )

    assertEquals(
      Functor[IdT[List, *]].fmap(IdT(List(Id(10))))(_ + 1).value,
      List(Id(11))
    )

    assertEquals(
      Functor[IdT[List, *]].fmap(IdT(List.empty[Id[Int]]))(_ + 1).value,
      Nil
    )
  }

  property("IdT Functor syntax") {
    import tech.backwards.fp.learn.Functor.syntax._

    val transformer: IdT[List, Int] =
      IdT(List(Id(10)))

    assertEquals(
      transformer.fmap(_ + 1).value,
      List(Id(11))
    )

    assertEquals(
      IdT(List(Id(10))).fmap(_ + 1).value,
      List(Id(11))
    )

    assertEquals(
      IdT(List(Id(10))) fmap (_ + 1),
      IdT(List(Id(11)))
    )

    assertEquals(
      IdT(List(Id(10))) `<$>` (_ + 1),
      IdT(List(Id(11)))
    )

    assertEquals(
      IdT(List.empty[Id[Int]]) `<$>` (_ + 1),
      IdT(List.empty[Id[Int]])
    )
  }

  property("IdT Monad") {
    val transformer: IdT[List, Int] =
      Monad[IdT[List, *]].pure(10)

    assertEquals(
      Monad[IdT[List, *]].flatMap(transformer)(a => Monad[IdT[List, *]].pure(a + 1)).value,
      List(Id(11))
    )

    assertEquals(
      Monad[IdT[List, *]].flatMap(Monad[IdT[List, *]].pure(10))(a => Monad[IdT[List, *]].pure(a + 1)).value,
      List(Id(11))
    )

    assertEquals(
      Monad[IdT[List, *]].flatMap(IdT(List.empty[Id[Int]]))(a => Monad[IdT[List, *]].pure(a + 1)).value,
      Nil
    )

    assertEquals(
      Monad[IdT[List, *]].flatMap(Monad[IdT[List, *]].pure(10))(_ => IdT(List.empty[Id[Int]])).value,
      Nil
    )
  }

  property("IdT Monad syntax") {
    import tech.backwards.fp.learn.Monad.syntax._

    val transformer: IdT[List, Int] =
      10.pure[IdT[List, *]]

    assertEquals(
      transformer.flatMap(a => (a + 1).pure[IdT[List, *]]).value,
      List(Id(11))
    )

    assertEquals(
      10.pure[IdT[List, *]].flatMap(a => (a + 1).pure[IdT[List, *]]).value,
      List(Id(11))
    )

    assertEquals(
      10.pure[IdT[List, *]] flatMap (a => IdT(List(Id(a + 1)))),
      IdT(List(Id(11)))
    )

    assertEquals(
      10.pure[IdT[List, *]] >>= (a => IdT(List(Id(a + 1)))),
      IdT(List(Id(11)))
    )

    assertEquals(
      IdT(List.empty[Id[Int]]) >>= (a => IdT(List(Id(a + 1)))),
      IdT(List.empty[Id[Int]])
    )

    assertEquals(
      10.pure[IdT[List, *]] >>= (_ => IdT(List.empty[Id[Int]])),
      IdT(List.empty[Id[Int]])
    )
  }

  property("IdT Applicative") {
    val transformerFn: IdT[List, Int => Int] =
      Applicative[IdT[List, *]].pure(_ + 1)

    val transformer: IdT[List, Int] =
      Applicative[IdT[List, *]].pure(10)

    assertEquals(
      Applicative[IdT[List, *]].ap(transformerFn)(transformer).value,
      List(Id(11))
    )

    assertEquals(
      Applicative[IdT[List, *]].ap(Applicative[IdT[List, *]].pure((x: Int) => x + 1))(Applicative[IdT[List, *]].pure(10)).value,
      List(Id(11))
    )

    assertEquals(
      Applicative[IdT[List, *]].ap(IdT(List.empty[Id[Int => Int]]))(Applicative[IdT[List, *]].pure(10)).value,
      Nil
    )

    assertEquals(
      Applicative[IdT[List, *]].ap(Applicative[IdT[List, *]].pure((x: Int) => x + 1))(IdT(List.empty[Id[Int]])).value,
      Nil
    )
  }

  property("IdT Applicative syntax") {
    import tech.backwards.fp.learn.Applicative.syntax._

    val transformerFn: IdT[List, Int => Int] =
      ((x: Int) => x + 1).pure[IdT[List, *]]

    val transformer: IdT[List, Int] =
      10.pure[IdT[List, *]]

    assertEquals(
      transformerFn.ap(transformer).value,
      List(Id(11))
    )

    assertEquals(
      ((x: Int) => x + 1).pure[IdT[List, *]].ap(10.pure[IdT[List, *]]).value,
      List(Id(11))
    )

    assertEquals(
      ((x: Int) => x + 1).pure[IdT[List, *]].ap(IdT(List.empty[Id[Int]])).value,
      Nil
    )

    assertEquals(
      IdT(List.empty[Id[Int => Int]]).ap(10.pure[IdT[List, *]]).value,
      Nil
    )

    assertEquals(
      IdT(List.empty[Id[Int => Int]]) ap 10.pure[IdT[List, *]],
      IdT(List.empty[Id[Int]])
    )

    assertEquals(
      IdT(List.empty[Id[Int => Int]]) <*> 10.pure[IdT[List, *]],
      IdT(List.empty[Id[Int]])
    )

    assertEquals(
      10.pure[IdT[List, *]].ap(((x: Int) => x + 1).pure[IdT[List, *]]).value,
      List(Id(11))
    )

    assertEquals(
      10.pure[IdT[List, *]].ap(IdT(List.empty[Id[Int => Int]])).value,
      Nil
    )

    assertEquals(
      IdT(List.empty[Id[Int]]).ap(((x: Int) => x + 1).pure[IdT[List, *]]).value,
      Nil
    )

    assertEquals(
      IdT(List.empty[Id[Int]]) ap ((x: Int) => x + 1).pure[IdT[List, *]],
      IdT(List.empty[Id[Int]])
    )

    assertEquals(
      IdT(List.empty[Id[Int]]) <*> ((x: Int) => x + 1).pure[IdT[List, *]],
      IdT(List.empty[Id[Int]])
    )
  }

  property("IdT Functor and Applicative syntax") {
    import tech.backwards.fp.learn.Applicative.syntax._
    import tech.backwards.fp.learn.Functor.syntax._

    assertEquals(
      IdT(List(Id(10))) `<$>` (x => (y: Int) => x + y) <*> IdT(List(Id(1))),
      IdT(List(Id(11)))
    )

    assertEquals(
      IdT(List.empty[Id[Int]]) `<$>` (x => (y: Int) => x + y) <*> IdT(List(Id(1))),
      IdT(List.empty[Id[Int]])
    )

    assertEquals(
      IdT(List(Id(10))) `<$>` (x => (y: Int) => x + y) <*> IdT(List.empty[Id[Int]]),
      IdT(List.empty[Id[Int]])
    )

    val add: Int => Int => Int =
      x => y => x + y

    assertEquals(
      IdT(List(Id(10))) `<$>` add <*> IdT(List.empty[Id[Int]]),
      IdT(List.empty[Id[Int]])
    )
  }

  property("IdT Functor and Applicative function syntax") {
    import tech.backwards.fp.learn.Applicative.syntax._
    import tech.backwards.fp.learn.Functor.syntax.function._

    assertEquals(
      ((x: Int) => (y: Int) => x + y) `<$>` IdT(List(Id(10))) <*> IdT(List(Id(1))),
      IdT(List(Id(11)))
    )

    assertEquals(
      ((x: Int) => (y: Int) => x + y) `<$>` IdT(List.empty[Id[Int]]) <*> IdT(List(Id(1))),
      IdT(List.empty[Id[Int]])
    )

    assertEquals(
      ((x: Int) => (y: Int) => x + y) `<$>` IdT(List(Id(10))) <*> IdT(List.empty[Id[Int]]),
      IdT(List.empty[Id[Int]])
    )

    val add: Int => Int => Int =
      x => y => x + y

    assertEquals(
      add `<$>` IdT(List(Id(10))) <*> IdT(List(Id(1))),
      IdT(List(Id(11)))
    )
  }

  property("IdT for comprehension") {
    import tech.backwards.fp.learn.Functor.syntax._
    import tech.backwards.fp.learn.Monad.syntax._

    val transformer: IdT[List, Int] =
      for {
        x <- 10.pure[IdT[List, *]]
        y <- 11.pure[IdT[List, *]]
        z <- 12.pure[IdT[List, *]]
      } yield x + y + z

    assertEquals(
      transformer.value,
      List(Id(33))
    )

    assertEquals(
      for {
        x <- IdT.lift(List(10))
        y <- IdT.lift(List(11))
        z <- IdT.lift(List(12))
        _ <- IdT(List(0).map(Id.apply)) // Without "lift"
      } yield x + y + z,
      IdT(List(Id(33)))
    )

    assertEquals(
      for {
        x <- IdT.lift(List(10))
        y <- IdT.lift(List.empty[Int])
        z <- IdT.lift(List(12))
      } yield x + y + z,
      IdT(List.empty[Id[Int]])
    )
  }
}