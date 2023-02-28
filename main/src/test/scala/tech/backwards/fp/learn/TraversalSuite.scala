package tech.backwards.fp.learn

import munit.ScalaCheckSuite
import org.scalacheck.Prop._
import org.scalacheck.Test

class TraversalSuite extends ScalaCheckSuite {
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

  property("Traverse List[Id]")(
    assertEquals(
      Traversal[List].traverse(List(1, 2, 3))(x => Id(x + 2)),
      Id(List(3, 4, 5))
    )
  )

  property("Traverse List[Id] syntax") {
    import tech.backwards.fp.learn.Traversal.syntax._

    assertEquals(
      List(1, 2, 3).traverse(x => Id(x + 2)),
      Id(List(3, 4, 5))
    )
  }

  property("Sequence List[Id] syntax") {
    import tech.backwards.fp.learn.Traversal.syntax._

    assertEquals(
      List(Id(1), Id(2), Id(3)).sequence,
      Id(List(1, 2, 3))
    )
  }

  property("Traverse Id[List]")(
    assertEquals(
      Traversal[Id].traverse(Id(1))(x => List(x + 1)),
      List(Id(2))
    )
  )

  property("Traverse Id[List] syntax") {
    import tech.backwards.fp.learn.Traversal.syntax._

    assertEquals(
      Id(1).traverse(x => List(x + 1)),
      List(Id(2))
    )
  }

  property("Sequence Id[List]") {
    import tech.backwards.fp.learn.Traversal.syntax._

    assertEquals(
      Id(List(1, 2, 3)).sequence,
      List(Id(1), Id(2), Id(3))
    )
  }

  property("Traverse List[List]")(
    assertEquals(
      Traversal[List].traverse(List(1, 2, 3)) {
        case 1 => List(2)
        case 2 => List(3, 4)
        case 3 => List(5, 6, 7)
      },
      List(
        List(2, 3, 5), List(2, 3, 6), List(2, 3, 7), List(2, 4, 5), List(2, 4, 6), List(2, 4, 7)
      )
    )
  )

  property("Traverse List[List] syntax") {
    import tech.backwards.fp.learn.Traversal.syntax._

    assertEquals(
      List(1, 2, 3) traverse {
        case 1 => List(2)
        case 2 => List(3, 4)
        case 3 => List(5, 6, 7)
      },
      List(
        List(2, 3, 5), List(2, 3, 6), List(2, 3, 7), List(2, 4, 5), List(2, 4, 6), List(2, 4, 7)
      )
    )
  }

  property("Sequence List[List] syntax") {
    import tech.backwards.fp.learn.Traversal.syntax._

    assertEquals(
      List(List(1), List(2, 3), List(4, 5, 6)).sequence,
      List(
        List(1, 2, 4), List(1, 2, 5), List(1, 2, 6), List(1, 3, 4), List(1, 3, 5), List(1, 3, 6)
      )
    )
  }

  property("Traverse Tuple2[List]")(
    assertEquals(
      Traversal[Lambda[X => (X, X)]].traverse((1, 3)) {
        case 1 => List(2, 3)
        case 3 => List(4)
      },
      List((2, 4), (3, 4))
    )
  )

  property("Traverse Tuple2[List] syntax") {
    import tech.backwards.fp.learn.Traversal.syntax._

    assertEquals(
      (1, 3) traverse {
        case 1 => List(2, 3)
        case 3 => List(4)
      },
      List((2, 4), (3, 4))
    )
  }

  property("Sequence Tuple2[List] syntax") {
    import tech.backwards.fp.learn.Traversal.syntax._

    assertEquals(
      (List(2, 3), List(4)).sequence,
      List((2, 4), (3, 4))
    )
  }

  property("Traverse Tuple3[List]")(
    assertEquals(
      Traversal[Lambda[X => (X, X, X)]].traverse((1, 3, 4)) {
        case 1 => List(2, 3)
        case 3 => List(4)
        case 4 => List(5, 6)
      },
      List((2, 4, 5), (2, 4, 6), (3, 4, 5), (3, 4, 6))
    )
  )

  property("Traverse Tuple3[List] syntax") {
    import tech.backwards.fp.learn.Traversal.syntax._

    assertEquals(
      (1, 3, 4) traverse {
        case 1 => List(2, 3)
        case 3 => List(4)
        case 4 => List(5, 6)
      },
      List((2, 4, 5), (2, 4, 6), (3, 4, 5), (3, 4, 6))
    )
  }

  property("Sequence Tuple3[List] syntax") {
    import tech.backwards.fp.learn.Traversal.syntax._

    assertEquals(
      (List(1, 2), List(3), List(4, 5)).sequence,
      List((1, 3, 4), (1, 3, 5), (2, 3, 4), (2, 3, 5))
    )
  }

  property("Traverse Id[Maybe]")(
    assertEquals(
      Traversal[Id].traverse(Id(5))(x => Just(x + 1)),
      Just(Id(6))
    )
  )

  property("Traverse Id[Maybe] syntax") {
    import tech.backwards.fp.learn.Traversal.syntax._

    assertEquals(
      Id(5).traverse(x => Just(x + 1)),
      Just(Id(6))
    )
  }

  property("Sequence Id[Maybe] syntax") {
    import tech.backwards.fp.learn.Traversal.syntax._

    assertEquals(
      Id(Just(5)).sequence,
      Just(Id(5))
    )
  }

  property("Traverse Maybe[Id]")(
    assertEquals(
      Traversal[Maybe].traverse(Just(5))(x => Id(x + 1)),
      Id(Just(6))
    )
  )

  property("Traverse Maybe[Id] syntax") {
    import tech.backwards.fp.learn.Traversal.syntax._

    assertEquals(
      Just(5).traverse(x => Id(x + 1)),
      Id(Just(6))
    )
  }

  property("Sequence Maybe[Id] syntax") {
    import tech.backwards.fp.learn.Traversal.syntax._

    assertEquals(
      Just(Id(5)).sequence,
      Id(Just(5))
    )
  }

  property("Traverse Tuple2[Maybe]") {
    assertEquals(
      Traversal[Lambda[X => (X, X)]].traverse(1, 2)(x => Just(x + 1)),
      Just(2, 3)
    )

    assertEquals(
      Traversal[Lambda[X => (X, X)]].traverse(1, 2) {
        case 2 => Nothing[Int]
        case x => Just(x + 1)
      },
      Nothing[(Int, Int)]
    )
  }

  property("Traverse Tuple2[Maybe] syntax") {
    import tech.backwards.fp.learn.Traversal.syntax._

    assertEquals(
      (1, 2).traverse(x => Just(x + 1)),
      Just(2, 3)
    )

    assertEquals(
      (1, 2) traverse {
        case 2 => Nothing[Int]
        case x => Just(x + 1)
      },
      Nothing[(Int, Int)]
    )
  }

  property("Sequence Tuple2[Maybe] syntax") {
    import tech.backwards.fp.learn.Traversal.syntax._

    assertEquals(
      (Just(1), Just(2)).sequence,
      Just(1, 2)
    )

    assertEquals(
      (Just(1), Nothing[Int]).sequence,
      Nothing[(Int, Int)]
    )
  }

  // Out of the blue I added Tuple1, when I was originally only working with Tuple2 and Tuple3
  property("Traverse Maybe[Tuple1]") {
    assertEquals(
      Traversal[Maybe].traverse(Just(1))(x => Tuple1(x + 1)),
      Tuple1(Just(2))
    )

    assertEquals(
      Traversal[Maybe].traverse(Nothing[Int])(x => Tuple1(x + 1)),
      Tuple1(Nothing[Int])
    )
  }

  property("Traverse Maybe[Tuple1] syntax") {
    import tech.backwards.fp.learn.Traversal.syntax._

    assertEquals(
      Just(1).traverse(x => Tuple1(x + 1)),
      Tuple1(Just(2))
    )

    assertEquals(
      Nothing[Int].traverse(x => Tuple1(x + 1)),
      Tuple1(Nothing[Int])
    )
  }

  property("Sequence Maybe[Tuple1] syntax") {
    import tech.backwards.fp.learn.Traversal.syntax._

    assertEquals(
      Just(Tuple1(1)).sequence,
      Tuple1(Just(1))
    )
  }

  property("Traverse Maybe[Tuple2]") {
    assertEquals(
      Traversal[Maybe].traverse[Lambda[X => (X, X)], Int, Int](Just(1))(x => (x + 1, x + 2)),
      (Just(2), Just(3))
    )

    assertEquals(
      Traversal[Maybe].traverse[Lambda[X => (X, X)], Int, String](Just(1))(_ => ("foo", "bar")),
      (Just("foo"), Just("bar"))
    )

    assertEquals(
      Traversal[Maybe].traverse[Lambda[X => (X, X)], Int, Int](Nothing[Int])(x => (x + 1, x + 2)),
      (Nothing[Int], Nothing[Int])
    )
  }

  property("Traverse Maybe[Tuple2] syntax") {
    import tech.backwards.fp.learn.Traversal.syntax._

    assertEquals(
      Just(1).traverse[Lambda[X => (X, X)], Int](x => (x + 1, x + 2)),
      (Just(2), Just(3))
    )

    assertEquals(
      Just(1).traverse[Lambda[X => (X, X)], String](_ => ("foo", "bar")),
      (Just("foo"), Just("bar"))
    )

    assertEquals(
      Nothing[Int].traverse[Lambda[X => (X, X)], Int](x => (x + 1, x + 2)),
      (Nothing[Int], Nothing[Int])
    )
  }

  property("Sequence Maybe[Tuple2] syntax") {
    import tech.backwards.fp.learn.Traversal.syntax._

    assertEquals(
      Just(1, 2).sequence,
      (Just(1), Just(2))
    )
  }

  property("Traverse Tuple3[Maybe]") {
    assertEquals(
      Traversal[Lambda[X => (X, X, X)]].traverse(1, 2, 3)(x => Just(x + 1)),
      Just(2, 3, 4)
    )

    assertEquals(
      Traversal[Lambda[X => (X, X, X)]].traverse(1, 2, 3) {
        case 2 => Nothing[Int]
        case x => Just(x + 1)
      },
      Nothing[(Int, Int, Int)]
    )
  }

  property("Traverse Tuple3[Maybe] syntax") {
    import tech.backwards.fp.learn.Traversal.syntax._

    assertEquals(
      (1, 2, 3).traverse(x => Just(x + 1)),
      Just(2, 3, 4)
    )

    assertEquals(
      (1, 2, 3) traverse {
        case 2 => Nothing[Int]
        case x => Just(x + 1)
      },
      Nothing[(Int, Int, Int)]
    )
  }

  property("Sequence Tuple3[Maybe] syntax") {
    import tech.backwards.fp.learn.Traversal.syntax._

    assertEquals(
      (Just(1), Just(2), Just(3)).sequence,
      Just(1, 2, 3)
    )

    assertEquals(
      (Just(1), Nothing[Int], Just(3)).sequence,
      Nothing[(Int, Int, Int)]
    )
  }

  property("Traverse Maybe[Tuple3]") {
    assertEquals(
      Traversal[Maybe].traverse[Lambda[X => (X, X, X)], Int, Int](Just(1))(x => (x + 1, x + 2, x + 3)),
      (Just(2), Just(3), Just(4))
    )

    assertEquals(
      Traversal[Maybe].traverse[Lambda[X => (X, X, X)], Int, String](Just(1))(_ => ("foo", "bar", "qux")),
      (Just("foo"), Just("bar"), Just("qux"))
    )

    assertEquals(
      Traversal[Maybe].traverse[Lambda[X => (X, X, X)], Int, Int](Nothing[Int])(x => (x + 1, x + 2, x + 3)),
      (Nothing[Int], Nothing[Int], Nothing[Int])
    )
  }

  property("Traverse Maybe[Tuple3] syntax") {
    import tech.backwards.fp.learn.Traversal.syntax._

    assertEquals(
      Just(1).traverse[Lambda[X => (X, X, X)], Int](x => (x + 1, x + 2, x + 3)),
      (Just(2), Just(3), Just(4))
    )

    assertEquals(
      Just(1).traverse[Lambda[X => (X, X, X)], String](_ => ("foo", "bar", "qux")),
      (Just("foo"), Just("bar"), Just("qux"))
    )

    assertEquals(
      Nothing[Int].traverse[Lambda[X => (X, X, X)], Int](x => (x + 1, x + 2, x + 3)),
      (Nothing[Int], Nothing[Int], Nothing[Int])
    )
  }

  property("Sequence Maybe[Tuple3] syntax") {
    import tech.backwards.fp.learn.Traversal.syntax._

    assertEquals(
      Just(1, 2, 3).sequence,
      (Just(1), Just(2), Just(3))
    )
  }

  property("Traverse List[Maybe]") {
    assertEquals(
      Traversal[List].traverse(List(1, 2, 3))(x => Just(x + 1)),
      Just(List(2, 3, 4))
    )

    assertEquals(
      Traversal[List].traverse(List(1, 2, 3)) {
        case 2 => Nothing[Int]
        case x => Just(x + 1)
      },
      Nothing[List[Int]]
    )
  }

  property("Traverse List[Maybe] syntax") {
    import tech.backwards.fp.learn.Traversal.syntax._

    assertEquals(
      List(1, 2, 3).traverse(x => Just(x + 1)),
      Just(List(2, 3, 4))
    )

    assertEquals(
      List(1, 2, 3) traverse {
        case 2 => Nothing[Int]
        case x => Just(x + 1)
      },
      Nothing[List[Int]]
    )
  }

  property("Sequence List[Maybe] syntax") {
    import tech.backwards.fp.learn.Traversal.syntax._

    assertEquals(
      List(Just(1), Just(2), Just(3)).sequence,
      Just(List(1, 2, 3))
    )

    assertEquals(
      List(Just(1), Nothing[Int], Just(3)).sequence,
      Nothing[List[Int]]
    )
  }

  property("Traverse Maybe[List]".ignore)(
  )

  property("Traverse Maybe[List] syntax".ignore)(
  )

  property("Sequence Maybe[List] syntax".ignore)(
  )
}