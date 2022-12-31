package tech.backwards.fp.learn

import munit.ScalaCheckSuite
import org.scalacheck.Prop._
import org.scalacheck.Test

class ListMonadSuite extends ScalaCheckSuite {
  override def scalaCheckTestParameters: Test.Parameters =
    super.scalaCheckTestParameters.withMinSuccessfulTests(100).withMaxDiscardRatio(10)

  property("List Monad pure")(
    assertEquals(
      Monad[List].pure(5),
      List(5)
    )
  )

  property("List Monad flatMap") {
    import tech.backwards.fp.Function.syntax._

    assertEquals(
      Monad[List].flatMap(List(1, 2, 3))(x => List(x + 1)),
      List(2, 3, 4)
    )

    assertEquals(
      Monad[List].flatMap(List(1, 2, 3))(_ + 1 |> Monad[List].pure),
      List(2, 3, 4)
    )
  }

  property("List Monad flatMap syntax") {
    import tech.backwards.fp.learn.Monad.syntax._

    assertEquals(
      List(1, 2, 3) >>= (x => List(x + 1)),
      List(2, 3, 4)
    )
  }

  property("List Monad pure and flatMap syntax") {
    import tech.backwards.fp.learn.Monad._
    import tech.backwards.fp.learn.Monad.syntax._

    assertEquals(
      5.pure >>= (x => List(x + 1)),
      6.pure
    )
  }

  property("List Monad flatMap and then map syntax") {
    import tech.backwards.fp.learn.Functor.syntax._
    import tech.backwards.fp.learn.Monad.syntax._

    assertEquals(
      (List(1, 2, 3) >>= (x => List(x + 1))) `<$>` (_ + 1),
      List(3, 4, 5)
    )
  }

  property("List Monad flatMap flattening syntax") {
    import tech.backwards.fp.learn.Monad.syntax._

    assertEquals(
      List(List(1, 2, 3), List(4, 5, 6)) >>= identity,
      (1 to 6).toList
    )
  }

  property("List Monad flatMap of arbitrary syntax") {
    import tech.backwards.fp.learn.Monad.syntax._

    forAll((xs: List[Int]) =>
      assertEquals(
        xs >>= (x => List(x + 1)),
        xs.map(_ + 1)
      )
    )
  }

  property("List Monad flatMap of function syntax") {
    import tech.backwards.fp.learn.Monad.syntax.function._

    assertEquals(
      { x: Int => List(x + 1) } flatMap List(1, 2, 3),
      List(2, 3, 4)
    )

    assertEquals(
      { x: Int => List(x + 1) } >>= List(1, 2, 3),
      List(2, 3, 4)
    )
  }
}