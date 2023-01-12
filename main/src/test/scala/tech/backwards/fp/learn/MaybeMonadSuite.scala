package tech.backwards.fp.learn

import munit.ScalaCheckSuite
import org.scalacheck.Prop._
import org.scalacheck.Test

class MaybeMonadSuite extends ScalaCheckSuite {
  override def scalaCheckTestParameters: Test.Parameters =
    super.scalaCheckTestParameters.withMinSuccessfulTests(100).withMaxDiscardRatio(10)

  property("Maybe Monad pure")(
    assertEquals(
      Monad[Maybe].pure(5),
      Just(5)
    )
  )

  property("Maybe Just Monad flatMap") {
    import tech.backwards.fp.Function.syntax._

    assertEquals(
      Monad[Maybe].flatMap(Just(5))(x => Just(x + 1)),
      Just(6)
    )

    assertEquals(
      Monad[Maybe].flatMap(Just(5))(_ + 1 |> Just.apply),
      Just(6)
    )
  }

  property("Maybe Just Monad flatMap syntax") {
    import tech.backwards.fp.learn.Monad.syntax._

    assertEquals(
      Just(5).flatMap(x => Just(x + 1)),
      Just(6)
    )
  }

  property("Maybe Just Monad pure and flatMap syntax") {
    import tech.backwards.fp.Function.syntax._
    import tech.backwards.fp.learn.Maybe._
    import tech.backwards.fp.learn.Monad.syntax._

    assertEquals(
      5.pure.flatMap(x => Just(x + 1)),
      6.pure
    )

    assertEquals(
      5.pure >>= (_ + 1 |> Just.apply),
      6.pure
    )
  }

  property("Maybe Nothing Monad flatMap") {
    assertEquals(
      Monad[Maybe].flatMap(Nothing[Int])(x => Just(x + 1)),
      Nothing[Int]
    )

    assertEquals(
      Monad[Maybe].flatMap(Just(5))(_ => Nothing[Int]),
      Nothing[Int]
    )
  }

  property("Maybe Just Monad flatMap and then map syntax") {
    import tech.backwards.fp.learn.Functor.syntax._
    import tech.backwards.fp.learn.Monad.syntax._

    assertEquals(
      Just(5).flatMap(x => Just(x + 1)).fmap(_ + 1),
      Just(7)
    )

    assertEquals(
      (Just(5) >>= (x => Just(x + 1))) `<$>` (_ + 1),
      Just(7)
    )
  }

  property("Maybe Just for comprehension syntax") {
    import tech.backwards.fp.learn.Functor.syntax._
    import tech.backwards.fp.learn.Monad.syntax._

    assertEquals(
      for {
        x <- Just(5)
        y <- Just(1)
        z <- Just(1)
      } yield x + y + z,
      Just(7)
    )
  }

  property("Maybe Nothing Monad flatMap and then map syntax") {
    import tech.backwards.fp.learn.Functor.syntax._
    import tech.backwards.fp.learn.Monad.syntax._

    assertEquals(
      Just(5).flatMap(_ => Nothing[Int]).fmap(_ + 1),
      Nothing[Int]
    )
  }

  property("Maybe Nothing for comprehension syntax") {
    import tech.backwards.fp.learn.Functor.syntax._
    import tech.backwards.fp.learn.Monad.syntax._

    assertEquals(
      for {
        x <- Just(5)
        y <- Nothing[Int]
        z <- Just(1)
      } yield x + y + z,
      Nothing[Int]
    )
  }

  property("Maybe Monad flatMap of arbitrary syntax") {
    import tech.backwards.fp.learn.Monad.syntax._

    forAll((x: Int) =>
      assertEquals(
        Just(x) >>= (x => Just(x + 1)),
        Just(x + 1)
      )
    )
  }

  property("Maybe Monad flatMap of function syntax") {
    import tech.backwards.fp.learn.Monad.syntax.function._

    assertEquals(
      ((x: Int) => Just(x + 1)) flatMap Just(5),
      Just(6)
    )

    assertEquals(
      ((x: Int) => Just(x + 1)) >>= Nothing[Int],
      Nothing[Int]
    )
  }
}