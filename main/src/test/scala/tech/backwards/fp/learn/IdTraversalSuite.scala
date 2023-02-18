package tech.backwards.fp.learn

import munit.ScalaCheckSuite
import org.scalacheck.Prop._
import org.scalacheck.Test

class IdTraversalSuite extends ScalaCheckSuite {
  override def scalaCheckTestParameters: Test.Parameters =
    super.scalaCheckTestParameters.withMinSuccessfulTests(100).withMaxDiscardRatio(10)

  property("Traverse Id[Id]")(
    assertEquals(
      Traversal[Id].traverse(Id(2))(x => Id(x * 2)),
      Id(Id(4))
    )
  )

  property("Traverse Id[Id] syntax") {
    import tech.backwards.fp.learn.Traversal.syntax._

    assertEquals(
      Id(2).traverse((x: Int) => Id(x * 2)),
      Id(Id(4))
    )
  }

  property("Sequence Id[Id] syntax") {
    import tech.backwards.fp.learn.Traversal.syntax._

    assertEquals(
      Id(Id(2)).sequence,
      Id(Id(2))
    )
  }

  property("Traverse Tuple2[Id]")(
    assertEquals(
      Traversal[Lambda[X => (X, X)]].traverse((1, 2))(x => Id(x * 2)),
      Id(2, 4)
    )
  )

  property("Traverse Tuple2[Id] syntax") {
    import tech.backwards.fp.learn.Traversal.syntax._

    assertEquals(
      (1, 2).traverse(x => Id(x * 2)),
      Id(2, 4)
    )
  }

  property("Sequence Tuple2[Id] syntax") {
    import tech.backwards.fp.learn.Traversal.syntax._

    assertEquals(
      (Id(1), Id(2)).sequence,
      Id(1, 2)
    )
  }

  property("Traverse Tuple3[Id]")(
    assertEquals(
      Traversal[Lambda[X => (X, X, X)]].traverse((1, 2, 3))(x => Id(x * 2)),
      Id(2, 4, 6)
    )
  )

  property("Traverse Tuple3[Id] syntax") {
    import tech.backwards.fp.learn.Traversal.syntax._

    assertEquals(
      (1, 2, 3).traverse(x => Id(x * 2)),
      Id(2, 4, 6)
    )
  }

  property("Sequence Tuple3[Id] syntax") {
    import tech.backwards.fp.learn.Traversal.syntax._

    assertEquals(
      (Id(1), Id(2), Id(3)).sequence,
      Id(1, 2, 3)
    )
  }

  import cats.Traverse
  import cats.Id

  // TODO - Traverse List of Id resulting in Id of List

  /*property("Id Monad flatMap")(
    assertEquals(
      Monad[Id].flatMap(Id(5))(x => Id(x + 1)),
      Id(6)
    )
  )*/

  /*property("Id Monad flatMap syntax") {
    import tech.backwards.fp.learn.Monad.syntax._

    assertEquals(
      Id(5).flatMap(x => Id(x + 1)),
      Id(6)
    )

    assertEquals(
      Id(5) >>= (x => Id(x + 1)),
      Id(6)
    )
  }*/

  /*property("Id Monad flatMap of arbitrary syntax") {
    import tech.backwards.fp.learn.Monad.syntax._

    forAll((x: Int) =>
      assertEquals(
        Id(x) >>= (x => Id(x + 1)),
        Id(x + 1)
      )
    )
  }*/

  /*property("Id Monad flatMap of function syntax") {
    import tech.backwards.fp.learn.Monad.syntax.function._

    assertEquals(
      ((x: Int) => Id(x + 1)) flatMap Id(5),
      Id(6)
    )

    assertEquals(
      ((x: Int) => Id(x + 1)) >>= Id(5),
      Id(6)
    )
  }*/
}