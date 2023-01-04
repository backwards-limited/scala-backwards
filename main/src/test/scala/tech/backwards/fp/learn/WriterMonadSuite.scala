package tech.backwards.fp.learn

import munit.ScalaCheckSuite
import org.scalacheck.Prop._
import org.scalacheck.Test

class WriterMonadSuite extends ScalaCheckSuite {
  override def scalaCheckTestParameters: Test.Parameters =
    super.scalaCheckTestParameters.withMinSuccessfulTests(100).withMaxDiscardRatio(10)

  /*property("Writer Monad pure")(
    assertEquals(
      Monad[Writer[String, *]].pure(5).run(),
      "" -> 5
    )
  )*/

  /*property("Disjunction Left Monad flatMap") {
    import tech.backwards.fp.Function.syntax._

    assertEquals(
      Monad[Disjunction[String, *]].flatMap(Left[String, Int]("foo"))(x => Right(x + 1)),
      Left[String, Int]("foo")
    )

    assertEquals(
      Monad[Disjunction[String, *]].flatMap(Left[String, Int]("foo"))(_ + 1 |> Right.apply),
      Left[String, Int]("foo")
    )
  }*/

  /*property("Disjunction Left Monad flatMap syntax") {
    import tech.backwards.fp.Function.syntax._
    import tech.backwards.fp.learn.Monad.syntax._

    assertEquals(
      Left[String, Int]("foo").flatMap(x => Right(x + 1)),
      Left[String, Int]("foo")
    )

    assertEquals(
      Left[String, Int]("foo") >>= (_ + 1 |> Right.apply),
      Left[String, Int]("foo")
    )

    assertEquals(
      Right[String, Int](5) >>= (_ => Left[String, Int]("foo")),
      Left[String, Int]("foo")
    )
  }*/

  /*property("Disjunction Right Monad flatMap") {
    import tech.backwards.fp.Function.syntax._

    assertEquals(
      Monad[Disjunction[String, *]].flatMap(Right(5))(x => Right(x + 1)),
      Right(6)
    )

    assertEquals(
      Monad[Disjunction[String, *]].flatMap(Right(5))(_ + 1 |> Right.apply),
      Right(6)
    )
  }*/

  /*property("Disjunction Right Monad flatMap syntax") {
    import tech.backwards.fp.Function.syntax._
    import tech.backwards.fp.learn.Monad.syntax._

    assertEquals(
      Right(5).flatMap(x => Right(x + 1)),
      Right(6)
    )

    assertEquals(
      Right(5) >>= (_ + 1 |> Right.apply),
      Right(6)
    )
  }*/

  /*property("Disjunction Right Monad pure and flatMap syntax") {
    import tech.backwards.fp.Function.syntax._
    import tech.backwards.fp.learn.Disjunction._
    import tech.backwards.fp.learn.Monad.syntax._

    assertEquals(
      5.pure.flatMap(x => Right(x + 1)),
      6.pure
    )

    assertEquals(
      5.pure >>= (_ + 1 |> Right.apply),
      6.pure
    )
  }*/

  /*property("Disjunction Right Monad flatMap and then map syntax") {
    import tech.backwards.fp.learn.Functor.syntax._
    import tech.backwards.fp.learn.Monad.syntax._

    assertEquals(
      Right(5).flatMap(x => Right(x + 1)).fmap(_ + 1),
      Right(7)
    )

    assertEquals(
      (Right(5) >>= (x => Right(x + 1))) `<$>` (_ + 1),
      Right(7)
    )
  }*/

  /*property("Disjunction right for comprehension syntax") {
    import tech.backwards.fp.learn.Functor.syntax._
    import tech.backwards.fp.learn.Monad.syntax._

    assertEquals(
      for {
        x <- 1.pure[Disjunction[String, *]]
        y <- 2.pure[Disjunction[String, *]]
        z <- 3.pure[Disjunction[String, *]]
      } yield x + y + z,
      Right(6)
    )
  }*/

  /*property("Disjunction Left for comprehension syntax") {
    import tech.backwards.fp.learn.Functor.syntax._
    import tech.backwards.fp.learn.Monad.syntax._

    assertEquals(
      for {
        x <- 1.pure[Disjunction[String, *]]
        y <- Left[String, Int]("foo")
        z <- 3.pure[Disjunction[String, *]]
      } yield x + y + z,
      Left("foo")
    )
  }*/

  /*property("Disjunction Monad flatMap of arbitrary syntax") {
    import tech.backwards.fp.learn.Monad.syntax._

    forAll((x: Int) =>
      assertEquals(
        Right(x) >>= (x => Right(x + 1)),
        Right(x + 1)
      )
    )
  }*/

  /*property("Disjunction Monad flatMap of function syntax") {
    import tech.backwards.fp.learn.Monad.syntax.function._

    assertEquals(
      { x: Int => Right(x + 1) } flatMap Right(5),
      Right(6)
    )

    assertEquals(
      { x: Int => Right[String, Int](x + 1) } >>= Left("foo"),
      Left("foo")
    )
  }*/
}