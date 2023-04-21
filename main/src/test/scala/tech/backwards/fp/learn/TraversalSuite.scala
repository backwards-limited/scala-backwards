package tech.backwards.fp.learn

import munit.ScalaCheckSuite
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

  property("Traverse Id[Maybe]") {
    assertEquals(
      Traversal[Id].traverse(Id(5))(x => Just(x + 1)),
      Just(Id(6))
    )

    assertEquals(
      Traversal[Id].traverse(Id(5))(_ => Nothing[Int]),
      Nothing[Id[Int]]
    )
  }

  property("Traverse Id[Maybe] syntax") {
    import tech.backwards.fp.learn.Traversal.syntax._

    assertEquals(
      Id(5).traverse(x => Just(x + 1)),
      Just(Id(6))
    )

    assertEquals(
      Id(5).traverse(_ => Nothing[Int]),
      Nothing[Id[Int]]
    )
  }

  property("Sequence Id[Maybe] syntax") {
    import tech.backwards.fp.learn.Traversal.syntax._

    assertEquals(
      Id(Just(5)).sequence,
      Just(Id(5))
    )

    assertEquals(
      Id(Nothing[Int]).sequence,
      Nothing[Id[Int]]
    )
  }

  property("Traverse Maybe[Id]") {
    assertEquals(
      Traversal[Maybe].traverse(Just(5))(x => Id(x + 1)),
      Id(Just(6))
    )

    assertEquals(
      Traversal[Maybe].traverse(Nothing[Int])(x => Id(x + 1)),
      Id(Nothing[Int])
    )
  }

  property("Traverse Maybe[Id] syntax") {
    import tech.backwards.fp.learn.Traversal.syntax._

    assertEquals(
      Just(5).traverse(x => Id(x + 1)),
      Id(Just(6))
    )

    assertEquals(
      Nothing[Int].traverse(x => Id(x + 1)),
      Id(Nothing[Int])
    )
  }

  property("Sequence Maybe[Id] syntax") {
    import tech.backwards.fp.learn.Traversal.syntax._

    assertEquals(
      Just(Id(5)).sequence,
      Id(Just(5))
    )

    assertEquals(
      Nothing[Id[Int]].sequence,
      Id(Nothing[Int])
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

    assertEquals(
      Nothing[Tuple1[Int]].sequence,
      Tuple1(Nothing[Int])
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

  property("Traverse Maybe[List]") {
    assertEquals(
      Traversal[Maybe].traverse(Just(1))(x => List(x)),
      List(Just(1))
    )

    assertEquals(
      Traversal[Maybe].traverse(Nothing[Int])(x => List(x)),
      List(Nothing[Int])
    )
  }

  property("Traverse Maybe[List] syntax") {
    import tech.backwards.fp.learn.Traversal.syntax._

    assertEquals(
      Just(1).traverse(x => List(x)),
      List(Just(1))
    )

    assertEquals(
      Nothing[Int].traverse(x => List(x)),
      List(Nothing[Int])
    )
  }

  property("Sequence Maybe[List] syntax") {
    import tech.backwards.fp.learn.Traversal.syntax._

    assertEquals(
      Just(List(1, 2)).sequence,
      List(Just(1), Just(2))
    )

    assertEquals(
      Nothing[List[Int]].sequence,
      List(Nothing[Int])
    )
  }

  property("Traverse Id[Right]")(
    assertEquals(
      Traversal[Id].traverse(Id(5))(x => Right(x + 1)),
      Right(Id(6))
    )
  )

  property("Traverse Id[Right] syntax") {
    import tech.backwards.fp.learn.Traversal.syntax._

    assertEquals(
      Id(5).traverse(x => Right(x + 1)),
      Right(Id(6))
    )
  }

  property("Sequence Id[Right] syntax") {
    import tech.backwards.fp.learn.Traversal.syntax._

    assertEquals(
      Id(Right(5)).sequence,
      Right(Id(5))
    )
  }

  property("Traverse Id[Left]") {
    assertEquals(
      Traversal[Id].traverse(Id(5))(_ => Left("a")),
      Left("a")
    )
  }

  property("Traverse Id[Left] syntax") {
    import tech.backwards.fp.learn.Traversal.syntax._

    assertEquals(
      Id(5).traverse(_ => Left("a")),
      Left("a")
    )
  }

  property("Sequence Id[Left] syntax") {
    import tech.backwards.fp.learn.Traversal.syntax._

    assertEquals(
      Id(Left[String, Int]("a")).sequence,
      Left("a")
    )
  }

  property("Traverse Right[Id]")(
    assertEquals(
      Traversal[Disjunction[String, *]].traverse(Right(5))(x => Id(x + 1)),
      Id(Right[String, Int](6))
    )
  )

  property("Traverse Right[Id] syntax") {
    import tech.backwards.fp.learn.Traversal.syntax._

    assertEquals(
      Right(5).traverse(x => Id(x + 1)),
      Id(Right(6))
    )
  }

  property("Sequence Right[Id] syntax") {
    import tech.backwards.fp.learn.Traversal.syntax._

    assertEquals(
      Right(Id(5)).sequence,
      Id(Right(5))
    )
  }

  property("Traverse Left[Id]") {
    import tech.backwards.fp.learn.Disjunction.syntax._

    assertEquals(
      Traversal[Disjunction[String, *]].traverse("a".left[Int])(x => Id(x + 1)),
      Id("a".left[Int])
    )
  }

  property("Traverse Left[Id] syntax") {
    import tech.backwards.fp.learn.Disjunction.syntax._
    import tech.backwards.fp.learn.Traversal.syntax._

    assertEquals(
      "a".left[Int].traverse(x => Id(x + 1)),
      Id("a".left[Int])
    )
  }

  property("Sequence Left[Id] syntax") {
    import tech.backwards.fp.learn.Disjunction.syntax._
    import tech.backwards.fp.learn.Traversal.syntax._

    assertEquals(
      "a".left[Id[Int]].sequence,
      Id("a".left[Int])
    )
  }

  property("Traverse List[Disjunction]") {
    assertEquals(
      Traversal[List].traverse(List(1, 2, 3))(x => Right(x + 1)),
      Right(List(2, 3, 4))
    )

    assertEquals(
      Traversal[List].traverse(List(1, 2, 3)) {
        case 2 => Left("a")
        case x => Right(x + 1)
      },
      Left("a")
    )
  }

  property("Traverse List[Disjunction] syntax") {
    import tech.backwards.fp.learn.Traversal.syntax._

    assertEquals(
      List(1, 2, 3).traverse(x => Right(x + 1)),
      Right(List(2, 3, 4))
    )

    assertEquals(
      List(1, 2, 3) traverse {
        case 2 => Left("a")
        case x => Right(x + 1)
      },
      Left("a")
    )
  }

  property("Sequence List[Disjunction] syntax") {
    import tech.backwards.fp.learn.Traversal.syntax._

    assertEquals(
      List(Right(1), Right(2), Right(3)).sequence,
      Right(List(1, 2, 3))
    )

    assertEquals(
      List(Right(1), Left("a"), Right(3)).sequence,
      Left("a")
    )

    assertEquals(
      List(Right(1), Left("a"), Left("b")).sequence,
      Left("a")
    )
  }

  property("Traverse Disjunction[List]") {
    assertEquals(
      Traversal[Disjunction[String, *]].traverse(Right(5))(x => List(x + 1)),
      List(Right(6))
    )

    assertEquals(
      Traversal[Disjunction[String, *]].traverse(Left[String, Int]("a"))(x => List(x + 1)),
      List(Left("a"))
    )
  }

  property("Traverse Disjunction[List] syntax") {
    import tech.backwards.fp.learn.Traversal.syntax._

    assertEquals(
      Right(5).traverse(x => List(x + 1)),
      List(Right(6))
    )

    assertEquals(
      Left[String, Int]("a").traverse(x => List(x + 1)),
      List(Left("a"))
    )
  }

  property("Sequence Disjunction[List] syntax") {
    import tech.backwards.fp.learn.Traversal.syntax._

    assertEquals(
      Right(List(5, 6)).sequence,
      List(Right(5), Right(6))
    )

    assertEquals(
      Left[String, List[Int]]("a").sequence,
      List(Left("a"))
    )
  }

  property("Traverse Tuple2[Disjunction]") {
    assertEquals(
      Traversal[Lambda[X => (X, X)]].traverse(1, 2)(x => Right(x + 1)),
      Right(2, 3)
    )

    assertEquals(
      Traversal[Lambda[X => (X, X)]].traverse(1, 2) {
        case 2 => Left("a")
        case x => Right(x + 1)
      },
      Left("a")
    )
  }

  property("Traverse Tuple2[Disjunction] syntax") {
    import tech.backwards.fp.learn.Traversal.syntax._

    assertEquals(
      (1, 2).traverse(x => Right(x + 1)),
      Right(2, 3)
    )

    assertEquals(
      (1, 2) traverse {
        case 2 => Left("a")
        case x => Right(x + 1)
      },
      Left("a")
    )
  }

  property("Sequence Tuple2[Disjunction] syntax") {
    import tech.backwards.fp.learn.Traversal.syntax._

    assertEquals(
      (Right(1), Right(2)).sequence,
      Right(1, 2)
    )

    assertEquals(
      (Right[String, Int](1), Left[String, Int]("a")).sequence,
      Left("a")
    )
  }

  property("Traverse Disjunction[Tuple2]") {
    assertEquals(
      Traversal[Disjunction[String, *]].traverse[Lambda[X => (X, X)], Int, Int](Right(1))(x => (x + 1, x + 2)),
      (Right(2), Right(3))
    )

    assertEquals(
      Traversal[Disjunction[String, *]].traverse[Lambda[X => (X, X)], Int, String](Right(1))(_ => ("foo", "bar")),
      (Right("foo"), Right("bar"))
    )

    assertEquals(
      Traversal[Disjunction[String, *]].traverse[Lambda[X => (X, X)], Int, Int](Left("a"))(x => (x + 1, x + 2)),
      (Left("a"), Left("a"))
    )
  }

  property("Traverse Disjunction[Tuple2] syntax") {
    import tech.backwards.fp.learn.Traversal.syntax._

    assertEquals(
      Right(1).traverse[Lambda[X => (X, X)], Int](x => (x + 1, x + 2)),
      (Right(2), Right(3))
    )

    assertEquals(
      Right(1).traverse[Lambda[X => (X, X)], String](_ => ("foo", "bar")),
      (Right("foo"), Right("bar"))
    )

    assertEquals(
      Left[String, Int]("a").traverse[Lambda[X => (X, X)], Int](x => (x + 1, x + 2)),
      (Left("a"), Left("a"))
    )
  }

  property("Sequence Disjunction[Tuple2] syntax") {
    import tech.backwards.fp.learn.Traversal.syntax._

    assertEquals(
      Right(1, 2).sequence,
      (Right(1), Right(2))
    )

    assertEquals(
      Left[String, (Int, Int)]("a").sequence,
      (Left("a"), Left("a"))
    )
  }

  property("Traverse Tuple3[Disjunction]") {
    assertEquals(
      Traversal[Lambda[X => (X, X, X)]].traverse((1, 2, 3))(x => Right(x + 1)),
      Right((2, 3, 4))
    )

    assertEquals(
      Traversal[Lambda[X => (X, X, X)]].traverse((1, 2, "3")) {
        case x: Int => Right(x + 1)
        case x: String => Right(x)
      },
      Right((2, 3, "3"))
    )

    assertEquals(
      Traversal[Lambda[X => (X, X, X)]].traverse((1, 2, 3)) {
        case 2 => Left("a")
        case x => Right(x + 1)
      },
      Left("a")
    )
  }

  property("Traverse Tuple3[Disjunction] syntax") {
    import tech.backwards.fp.learn.Traversal.syntax._

    assertEquals(
      (1, 2, 3).traverse(x => Right(x + 1)),
      Right(2, 3, 4)
    )

    assertEquals(
      (1, 2, "3") traverse {
        case x: Int => Right(x + 1)
        case x: String => Right(x)
      },
      Right(2, 3, "3")
    )

    assertEquals(
      (1, 2, 3) traverse {
        case 2 => Left("a")
        case x => Right(x + 1)
      },
      Left("a")
    )
  }

  property("Sequence Tuple3[Disjunction] syntax") {
    import tech.backwards.fp.learn.Traversal.syntax._

    assertEquals(
      (Right(1), Right(2), Right(3)).sequence,
      Right(1, 2, 3)
    )

    assertEquals(
      (Right[String, Any](1), Right[String, Any](2), Right[String, Any]("3")).sequence,
      Right(1, 2, "3")
    )

    assertEquals(
      (Right[String, Int](1), Left[String, Int]("a"), Right[String, Int](3)).sequence,
      Left("a")
    )
  }

  property("Traverse Disjunction[Tuple3]") {
    assertEquals(
      Traversal[Disjunction[String, *]].traverse[Lambda[X => (X, X, X)], Int, Int](Right(1))(x => (x + 1, x + 2, x + 3)),
      (Right(2), Right(3), Right(4))
    )

    assertEquals(
      Traversal[Disjunction[String, *]].traverse[Lambda[X => (X, X, X)], Int, Any](Right(1))(x => (x, "foo", "bar")),
      (Right(1), Right("foo"), Right("bar"))
    )

    assertEquals(
      Traversal[Disjunction[String, *]].traverse[Lambda[X => (X, X, X)], Int, Int](Left("a"))(x => (x + 1, x + 2, x + 3)),
      (Left("a"), Left("a"), Left("a"))
    )
  }

  property("Traverse Disjunction[Tuple3] syntax") {
    import tech.backwards.fp.learn.Traversal.syntax._

    assertEquals(
      Right(1).traverse[Lambda[X => (X, X, X)], Int](x => (x + 1, x + 2, x + 3)),
      (Right(2), Right(3), Right(4))
    )

    assertEquals(
      Right(1).traverse[Lambda[X => (X, X, X)], String](_ => ("foo", "bar", "baz")),
      (Right("foo"), Right("bar"), Right("baz"))
    )

    assertEquals(
      Left[String, Int]("a").traverse[Lambda[X => (X, X, X)], Int](x => (x + 1, x + 2, x + 3)),
      (Left("a"), Left("a"), Left("a"))
    )
  }

  property("Sequence Disjunction[Tuple3] syntax") {
    import tech.backwards.fp.learn.Traversal.syntax._

    assertEquals(
      Right(1, 2, 3).sequence,
      (Right(1), Right(2), Right(3))
    )

    assertEquals(
      Right(1, 2, "3").sequence,
      (Right(1), Right(2), Right("3"))
    )

    assertEquals(
      Left[String, (Int, Int, Int)]("a").sequence,
      (Left("a"), Left("a"), Left("a"))
    )

    assertEquals(
      Left[String, (Int, Int, String)]("a").sequence,
      (Left("a"), Left("a"), Left("a"))
    )
  }

  property("Traverse Maybe[Disjunction]") {
    assertEquals(
      Traversal[Maybe].traverse(Just(1))(Right.apply),
      Right(Just(1))
    )

    assertEquals(
      Traversal[Maybe].traverse(Just(1))(_ => Left("a")),
      Left("a")
    )

    assertEquals(
      Traversal[Maybe].traverse(Nothing[Int])(Right.apply),
      Right(Nothing[Int])
    )

    assertEquals(
      Traversal[Maybe].traverse(Nothing[Int])(_ => Left("a")),
      Right[String, Maybe[Int]](Nothing[Int]) // One might expect Left("a"), but "worksheet" with Cats version asserts this.
    )
  }

  property("Traverse Maybe[Disjunction] syntax") {
    import tech.backwards.fp.learn.Traversal.syntax._

    assertEquals(
      Just(1) traverse Right.apply,
      Right(Just(1))
    )

    assertEquals(
      Just(1).traverse(_ => Left("a")),
      Left("a")
    )

    assertEquals(
      Nothing[Int].traverse(Right.apply),
      Right(Nothing[Int])
    )

    assertEquals(
      Nothing[Int].traverse(_ => Left("a")),
      Right[String, Maybe[Int]](Nothing[Int]) // One might expect Left("a"), but "worksheet" with Cats version asserts this.
    )
  }

  property("Sequence Maybe[Disjunction] syntax") {
    import tech.backwards.fp.learn.Traversal.syntax._

    assertEquals(
      Just(Right(1)).sequence,
      Right(Just(1))
    )

    assertEquals(
      Just(Left("a")).sequence,
      Left("a")
    )

    assertEquals(
      Nothing[Disjunction[String, Int]].sequence,
      Right(Nothing[Int])
    )
  }

  property("Traverse Disjunction[Maybe]") {
    assertEquals(
      Traversal[Disjunction[String, *]].traverse(Right(1))(Just.apply),
      Just(Right(1))
    )

    assertEquals(
      Traversal[Disjunction[String, *]].traverse(Right(1))(_ => Nothing[Int]),
      Nothing[Disjunction[String, Int]]
    )

    assertEquals(
      Traversal[Disjunction[String, *]].traverse(Left[String, Int]("a"))(Just.apply),
      Just(Left("a"))
    )

    assertEquals(
      Traversal[Disjunction[String, *]].traverse(Left[String, Int]("a"))(_ => Nothing[Int]),
      Just(Left("a")) // One might expect Nothing[Disjunction[String, Int]], but "worksheet" with Cats version asserts this.
    )
  }

  property("Traverse Disjunction[Maybe] syntax") {
    import tech.backwards.fp.learn.Traversal.syntax._

    assertEquals(
      Right(1) traverse Just.apply,
      Just(Right(1))
    )

    assertEquals(
      Right(1).traverse(_ => Nothing[Int]),
      Nothing[Disjunction[String, Int]]
    )

    assertEquals(
      Left[String, Int]("a") traverse Just.apply,
      Just(Left("a"))
    )

    assertEquals(
      Left[String, Int]("a").traverse(_ => Nothing[Int]),
      Just(Left("a")) // One might expect Nothing[Disjunction[String, Int]], but "worksheet" with Cats version asserts this.
    )
  }

  property("Sequence Disjunction[Maybe] syntax") {
    import tech.backwards.fp.learn.Traversal.syntax._

    assertEquals(
      Right(Just(1)).sequence,
      Just(Right(1))
    )

    assertEquals(
      Right(Nothing[Int]).sequence,
      Nothing[Disjunction[String, Int]]
    )

    assertEquals(
      Left[String, Maybe[Int]]("a").sequence,
      Just(Left("a"))
    )
  }

  property("Traverse Id[State]") {
    assertEquals(
      Traversal[Id].traverse(Id(1))(x => State[String, Int](_ + "bar" -> (x + 5))).run("foo"),
      "foobar" -> Id(6)
    )

    assertEquals(
      Traversal[Id].traverse(Id(1))(x => State[List[String], Int](_ ++ List("bar") -> (x + 5))).run(List("foo")),
      List("foo", "bar") -> Id(6)
    )
  }

  property("Traverse Id[State] syntax") {
    import tech.backwards.fp.learn.Traversal.syntax._

    assertEquals(
      Id(1) traverse(x => State[String, Int](_ + "bar" -> (x + 5))) run "foo",
      "foobar" -> Id(6)
    )

    assertEquals(
      Id(1) traverse(x => State[List[String], Int](_ ++ List("bar") -> (x + 5))) run List("foo"),
      List("foo", "bar") -> Id(6)
    )
  }

  property("Sequence Id[State] syntax") {
    import tech.backwards.fp.learn.Traversal.syntax._

    assertEquals(
      Id(State[String, Int](_ + "bar" -> 5)).sequence.run("foo"),
      "foobar" -> Id(5)
    )

    assertEquals(
      Id(State[List[String], Int](_ ++ List("bar") -> 5)).sequence.run(List("foo")),
      List("foo", "bar") -> Id(5)
    )
  }

  // Quite a lot going on here, so we won't test Tuple3[State] (We didn't really need to for all others, but that's ok).
  property("Traverse Tuple2[State]") {
    assertEquals(
      Traversal[Lambda[X => (X, X)]].traverse((1, 2))(x => State[String, Int](_ + "bar" -> (x + 5))).run("foo"),
      "foobarbar" -> (6, 7)
    )

    assertEquals(
      Traversal[Lambda[X => (X, X)]].traverse((1, 2)) {
        case x @ 1 =>
          State[String, Int](_ + "bar" -> (x + 5))
        case x @ 2 =>
          State[String, Int](_ + "baz" -> (x + 5))
      } run "foo",
      "foobarbaz" -> (6, 7)
    )

    assertEquals(
      Traversal[Lambda[X => (X, X)]].traverse((1, 2))(x => State[List[String], Int](_ ++ List("bar") -> (x + 5))).run(List("foo")),
      List("foo", "bar", "bar") -> (6, 7)
    )

    assertEquals(
      Traversal[Lambda[X => (X, X)]].traverse((1, 2)) {
        case x @ 1 =>
          State[List[String], Int](_ ++ List("bar") -> (x + 5))
        case x @ 2 =>
          State[List[String], Int](_ ++ List("baz") -> (x + 5))
      } run List("foo"),
      List("foo", "bar", "baz") -> (6, 7)
    )
  }

  property("Traverse Tuple2[State] syntax") {
    import tech.backwards.fp.learn.Traversal.syntax._

    assertEquals(
      (1, 2).traverse(x => State[String, Int](_ + "bar" -> (x + 5))).run("foo"),
      "foobarbar" -> (6, 7)
    )

    assertEquals(
      (1, 2).traverse {
        case x @ 1 =>
          State[String, Int](_ + "bar" -> (x + 5))
        case x @ 2 =>
          State[String, Int](_ + "baz" -> (x + 5))
      } run "foo",
      "foobarbaz" -> (6, 7)
    )

    assertEquals(
      (1, 2).traverse(x => State[List[String], Int](_ ++ List("bar") -> (x + 5))).run(List("foo")),
      List("foo", "bar", "bar") -> (6, 7)
    )

    assertEquals(
      (1, 2).traverse {
        case x @ 1 =>
          State[List[String], Int](_ ++ List("bar") -> (x + 5))
        case x @ 2 =>
          State[List[String], Int](_ ++ List("baz") -> (x + 5))
      } run List("foo"),
      List("foo", "bar", "baz") -> (6, 7)
    )
  }

  property("Sequence Tuple2[State] syntax") {
    import tech.backwards.fp.learn.Traversal.syntax._

    assertEquals(
      (State[String, Int](_ + "bar" -> 1), State[String, Int](_ + "bar" -> 2)).sequence.run("foo"),
      "foobarbar" -> (1, 2)
    )

    assertEquals(
      (State((s: String) => s + "bar" -> 1), State((s: String) => s + "baz" -> 2)).sequence.run("foo"),
      "foobarbaz" -> (1, 2)
    )

    assertEquals(
      (State((_: String) + "bar" -> 1), State((_: String) + "boo" -> 2)).sequence.run("foo"),
      "foobarboo" -> (1, 2)
    )

    assertEquals(
      (State[List[String], Int](_ ++ List("bar") -> 1), State[List[String], Int](_ ++ List("bar") -> 2)).sequence.run(List("foo")),
      List("foo", "bar", "bar") -> (1, 2)
    )

    assertEquals(
      (State((xs: List[String]) => xs ++ List("bar") -> 1), State((xs: List[String]) => xs ++ List("baz") -> 2)).sequence.run(List("foo")),
      List("foo", "bar", "baz") -> (1, 2)
    )

    assertEquals(
      (State((_: List[String]) ++ List("bar") -> 1), State((_: List[String]) ++ List("boo") -> 2)).sequence.run(List("foo")),
      List("foo", "bar", "boo") -> (1, 2)
    )
  }

  property("Traverse List[State]") {
    import tech.backwards.fp.learn.Applicative.syntax._

    assertEquals(
      Traversal[List].traverse(List(1, 2, 3))(x => State[String, Int](_ + "bar" -> (x + 5))).run("foo"),
      "foobarbarbar" -> List(6, 7, 8)
    )

    assertEquals(
      Traversal[List].traverse(List(1, 2, 3)) {
        case x @ 2 =>
          State[String, Int](_ + "bar" -> (x + 10))
        case x =>
          (x + 1).pure[State[String, *]]
      } run "foo",
      "foobar" -> List(2, 12, 4)
    )

    assertEquals(
      Traversal[List].traverse(List(1, 2, 3))(x => State[List[String], Int](_ ++ List("bar") -> (x + 5))).run(List("foo")),
      List("foo", "bar", "bar", "bar") -> List(6, 7, 8)
    )

    assertEquals(
      Traversal[List].traverse(List(1, 2, 3)) {
        case x @ 2 =>
          State[List[String], Int](_ ++ List("bar") -> (x + 10))
        case x =>
          (x + 1).pure[State[List[String], *]]
      } run List("foo"),
      List("foo", "bar") -> List(2, 12, 4)
    )
  }

  property("Traverse List[State] syntax") {
    import tech.backwards.fp.learn.Applicative.syntax._
    import tech.backwards.fp.learn.Traversal.syntax._

    assertEquals(
      List(1, 2, 3).traverse(x => State[String, Int](_ + "bar" -> (x + 5))).run("foo"),
      "foobarbarbar" -> List(6, 7, 8)
    )

    assertEquals(
      List(1, 2, 3).traverse {
        case x @ 2 =>
          State[String, Int](_ + "bar" -> (x + 10))
        case x =>
          (x + 1).pure[State[String, *]]
      } run "foo",
      "foobar" -> List(2, 12, 4)
    )

    assertEquals(
      List(1, 2, 3).traverse(x => State[List[String], Int](_ ++ List("bar") -> (x + 5))).run(List("foo")),
      List("foo", "bar", "bar", "bar") -> List(6, 7, 8)
    )

    assertEquals(
      List(1, 2, 3).traverse {
        case x @ 2 =>
          State[List[String], Int](_ ++ List("bar") -> (x + 10))
        case x =>
          (x + 1).pure[State[List[String], *]]
      } run List("foo"),
      List("foo", "bar") -> List(2, 12, 4)
    )
  }

  property("Sequence List[State] syntax") {
    import tech.backwards.fp.learn.Applicative.syntax._
    import tech.backwards.fp.learn.Traversal.syntax._

    assertEquals(
      List(1.pure[State[String, *]], State[String, Int](_ + "bar" -> 2), 3.pure[State[String, *]]).sequence.run("foo"),
      "foobar" -> List(1, 2, 3)
    )

    assertEquals(
      List(1.pure[State[List[String], *]], State[List[String], Int](_ ++ List("bar") -> 2), 3.pure[State[List[String], *]]).sequence.run(List("foo")),
      List("foo", "bar") -> List(1, 2, 3)
    )
  }

  property("Traverse Maybe[State]") {
    assertEquals(
      Traversal[Maybe].traverse(Just(1))(x => State[String, Int](_ + "bar" -> (x + 5))).run("foo"),
      "foobar" -> Just(6)
    )

    assertEquals(
      Traversal[Maybe].traverse(Nothing[Int])(x => State[String, Int](_ + "bar" -> (x + 5))).run("foo"),
      "foo" -> Nothing[Int]
    )

    assertEquals(
      Traversal[Maybe].traverse(Just(1))(x => State[List[String], Int](_ ++ List("bar") -> (x + 5))).run(List("foo")),
      List("foo", "bar") -> Just(6)
    )

    assertEquals(
      Traversal[Maybe].traverse(Nothing[Int])(x => State[List[String], Int](_ ++ List("bar") -> (x + 5))).run(List("foo")),
      List("foo") -> Nothing[Int]
    )
  }

  property("Traverse Maybe[State] syntax") {
    import tech.backwards.fp.learn.Traversal.syntax._

    assertEquals(
      Just(1).traverse(x => State[String, Int](_ + "bar" -> (x + 5))).run("foo"),
      "foobar" -> Just(6)
    )

    assertEquals(
      Nothing[Int].traverse(x => State[String, Int](_ + "bar" -> (x + 5))).run("foo"),
      "foo" -> Nothing[Int]
    )

    assertEquals(
      Just(1).traverse(x => State[List[String], Int](_ ++ List("bar") -> (x + 5))).run(List("foo")),
      List("foo", "bar") -> Just(6)
    )

    assertEquals(
      Nothing[Int].traverse(x => State[List[String], Int](_ ++ List("bar") -> (x + 5))).run(List("foo")),
      List("foo") -> Nothing[Int]
    )
  }

  property("Sequence Maybe[State] syntax") {
    import tech.backwards.fp.learn.Traversal.syntax._

    assertEquals(
      Just(State[String, Int](_ + "bar" -> 1)).sequence.run("foo"),
      "foobar" -> Just(1)
    )

    assertEquals(
      Nothing[State[String, Int]].sequence.run("foo"),
      "foo" -> Nothing[Int]
    )

    assertEquals(
      Just(State[List[String], Int](_ ++ List("bar") -> 1)).sequence.run(List("foo")),
      List("foo", "bar") -> Just(1)
    )

    assertEquals(
      Nothing[State[List[String], Int]].sequence.run(List("foo")),
      List("foo") -> Nothing[Int]
    )
  }

  property("Traverse Disjunction[State]") {
    assertEquals(
      Traversal[Disjunction[String, *]].traverse(Right(1))(x => State[String, Int](_ + "bar" -> (x + 5))).run("foo"),
      "foobar" -> Right(6)
    )

    assertEquals(
      Traversal[Disjunction[String, *]].traverse(Left[String, Int]("a"))(x => State[String, Int](_ + "bar" -> (x + 5))).run("foo"),
      "foo" -> Left("a")
    )

    assertEquals(
      Traversal[Disjunction[String, *]].traverse(Right(1))(x => State[List[String], Int](_ ++ List("bar") -> (x + 5))).run(List("foo")),
      List("foo", "bar") -> Right(6)
    )

    assertEquals(
      Traversal[Disjunction[String, *]].traverse(Left[String, Int]("a"))(x => State[List[String], Int](_ ++ List("bar") -> (x + 5))).run(List("foo")),
      List("foo") -> Left("a")
    )
  }

  property("Traverse Disjunction[State] syntax") {
    import tech.backwards.fp.learn.Traversal.syntax._

    assertEquals(
      Right(1).traverse(x => State[String, Int](_ + "bar" -> (x + 5))).run("foo"),
      "foobar" -> Right(6)
    )

    assertEquals(
      Left[String, Int]("a").traverse(x => State[String, Int](_ + "bar" -> (x + 5))).run("foo"),
      "foo" -> Left("a")
    )

    assertEquals(
      Right(1).traverse(x => State[List[String], Int](_ ++ List("bar") -> (x + 5))).run(List("foo")),
      List("foo", "bar") -> Right(6)
    )

    assertEquals(
      Left[String, Int]("a").traverse(x => State[List[String], Int](_ ++ List("bar") -> (x + 5))).run(List("foo")),
      List("foo") -> Left("a")
    )
  }

  property("Sequence Disjunction[State] syntax") {
    import tech.backwards.fp.learn.Traversal.syntax._

    assertEquals(
      Right(State[String, Int](_ + "bar" -> 1)).sequence.run("foo"),
      "foobar" -> Right(1)
    )

    assertEquals(
      Left[String, State[String, Int]]("a").sequence.run("foo"),
      "foo" -> Left("a")
    )

    assertEquals(
      Right(State[List[String], Int](_ ++ List("bar") -> 1)).sequence.run(List("foo")),
      List("foo", "bar") -> Right(1)
    )

    assertEquals(
      Left[String, State[List[String], Int]]("a").sequence.run(List("foo")),
      List("foo") -> Left("a")
    )
  }

  property("Traverse Id[Writer]") {
    assertEquals(
      Traversal[Id].traverse(Id(1))(x => Writer("foo" -> (x + 5))).run(),
      "foo" -> Id(6)
    )

    assertEquals(
      Traversal[Id].traverse(Id(1))(x => Writer(List("foo") -> (x + 5))).run(),
      List("foo") -> Id(6)
    )
  }

  property("Traverse Id[Writer] syntax") {
    import tech.backwards.fp.learn.Traversal.syntax._

    assertEquals(
      Id(1).traverse(x => Writer("foo" -> (x + 5))).run(),
      "foo" -> Id(6)
    )

    assertEquals(
      Id(1).traverse(x => Writer(List("foo") -> (x + 5))).run(),
      List("foo") -> Id(6)
    )
  }

  property("Sequence Id[Writer] syntax") {
    import tech.backwards.fp.learn.Traversal.syntax._

    assertEquals(
      Id(Writer("foo" -> 1)).sequence.run(),
      "foo" -> Id(1)
    )

    assertEquals(
      Id(Writer(List("foo") -> 1)).sequence.run(),
      List("foo") -> Id(1)
    )
  }

  // Quite a lot going on here, so we won't test Tuple3[Writer] (We didn't really need to for all others, but that's ok).
  property("Traverse Tuple2[Writer]") {
    assertEquals(
      Traversal[Lambda[X => (X, X)]].traverse((1, 2))(x => Writer("foo" -> (x + 5))).run(),
      "foofoo" -> (6, 7)
    )

    assertEquals(
      Traversal[Lambda[X => (X, X)]].traverse((1, 2)) {
        case x @ 1 =>
          Writer("foo" -> (x + 5))
        case x @ 2 =>
          Writer("bar" -> (x + 15))
      }.run(),
      "foobar" -> (6, 17)
    )

    assertEquals(
      Traversal[Lambda[X => (X, X)]].traverse((1, 2))(x => Writer(List("foo") -> (x + 5))).run(),
      List("foo", "foo") -> (6, 7)
    )

    assertEquals(
      Traversal[Lambda[X => (X, X)]].traverse((1, 2)) {
        case x @ 1 =>
          Writer(List("foo") -> (x + 5))
        case x @ 2 =>
          Writer(List("bar") -> (x + 15))
      }.run(),
      List("foo", "bar") -> (6, 17)
    )
  }

  property("Traverse Tuple2[Writer] syntax") {
    import tech.backwards.fp.learn.Traversal.syntax._

    assertEquals(
      (1, 2).traverse(x => Writer("foo" -> (x + 5))).run(),
      "foofoo" -> (6, 7)
    )

    assertEquals(
      (1, 2).traverse {
        case x @ 1 =>
          Writer("foo" -> (x + 5))
        case x @ 2 =>
          Writer("bar" -> (x + 15))
      }.run(),
      "foobar" -> (6, 17)
    )

    assertEquals(
      (1, 2).traverse(x => Writer(List("foo") -> (x + 5))).run(),
      List("foo", "foo") -> (6, 7)
    )

    assertEquals(
      (1, 2).traverse {
        case x @ 1 =>
          Writer(List("foo") -> (x + 5))
        case x @ 2 =>
          Writer(List("bar") -> (x + 15))
      }.run(),
      List("foo", "bar") -> (6, 17)
    )
  }

  property("Sequence Tuple2[Writer] syntax") {
    import tech.backwards.fp.learn.Traversal.syntax._

    assertEquals(
      (Writer("foo" -> 5), Writer("bar" -> 6)).sequence.run(),
      "foobar" -> (5, 6)
    )

    assertEquals(
      (Writer(List("foo") -> 5), Writer(List("bar") -> 6)).sequence.run(),
      List("foo", "bar") -> (5, 6)
    )
  }

  property("Traverse List[Writer]") {
    import tech.backwards.fp.learn.Applicative.syntax._

    assertEquals(
      Traversal[List].traverse(List(1, 2, 3))(x => Writer("foo" -> (x + 5))).run(),
      "foofoofoo" -> List(6, 7, 8)
    )

    assertEquals(
      Traversal[List].traverse(List(1, 2, 3)) {
        case x @ 1 =>
          Writer("foo" -> (x + 10))
        case x @ 2 =>
          (x + 1).pure[Writer[String, *]]
        case x =>
          Writer("baz" -> (x + 100))
      }.run(),
      "foobaz" -> List(11, 3, 103)
    )

    assertEquals(
      Traversal[List].traverse(List(1, 2, 3))(x => Writer(List("foo") -> (x + 5))).run(),
      List("foo", "foo", "foo") -> List(6, 7, 8)
    )

    assertEquals(
      Traversal[List].traverse(List(1, 2, 3)) {
        case x @ 1 =>
          Writer(List("foo") -> (x + 10))
        case x @ 2 =>
          (x + 1).pure[Writer[List[String], *]]
        case x =>
          Writer(List("baz") -> (x + 100))
      }.run(),
      List("foo", "baz") -> List(11, 3, 103)
    )
  }

  property("Traverse List[Writer] syntax") {
    import tech.backwards.fp.learn.Applicative.syntax._
    import tech.backwards.fp.learn.Traversal.syntax._

    assertEquals(
      List(1, 2, 3).traverse(x => Writer("foo" -> (x + 5))).run(),
      "foofoofoo" -> List(6, 7, 8)
    )

    assertEquals(
      List(1, 2, 3).traverse {
        case x @ 1 =>
          Writer("foo" -> (x + 10))
        case x @ 2 =>
          (x + 1).pure[Writer[String, *]]
        case x =>
          Writer("baz" -> (x + 100))
      }.run(),
      "foobaz" -> List(11, 3, 103)
    )

    assertEquals(
      List(1, 2, 3).traverse(x => Writer(List("foo") -> (x + 5))).run(),
      List("foo", "foo", "foo") -> List(6, 7, 8)
    )

    assertEquals(
      List(1, 2, 3).traverse {
        case x @ 1 =>
          Writer(List("foo") -> (x + 10))
        case x @ 2 =>
          (x + 1).pure[Writer[List[String], *]]
        case x =>
          Writer(List("baz") -> (x + 100))
      }.run(),
      List("foo", "baz") -> List(11, 3, 103)
    )
  }

  property("Sequence List[Writer] syntax") {
    import tech.backwards.fp.learn.Applicative.syntax._
    import tech.backwards.fp.learn.Traversal.syntax._

    assertEquals(
      List(Writer("foo" -> 1), 2.pure[Writer[String, *]], Writer("baz" -> 3)).sequence.run(),
      "foobaz" -> List(1, 2, 3)
    )

    assertEquals(
      List(Writer(List("foo") -> 1), 2.pure[Writer[List[String], *]], Writer(List("baz") -> 3)).sequence.run(),
      List("foo", "baz") -> List(1, 2, 3)
    )
  }

  property("Traverse Maybe[Writer]") {
    assertEquals(
      Traversal[Maybe].traverse(Just(1))(x => Writer("foo" -> (x + 5))).run(),
      "foo" -> Just(6)
    )

    assertEquals(
      Traversal[Maybe].traverse(Nothing[Int])(x => Writer("foo" -> (x + 5))).run(),
      "" -> Nothing[Int]
    )

    assertEquals(
      Traversal[Maybe].traverse(Just(1))(x => Writer(List("foo") -> (x + 5))).run(),
      List("foo") -> Just(6)
    )

    assertEquals(
      Traversal[Maybe].traverse(Nothing[Int])(x => Writer(List("foo") -> (x + 5))).run(),
      Nil -> Nothing[Int]
    )
  }

  property("Traverse Maybe[Writer] syntax") {
    import tech.backwards.fp.learn.Traversal.syntax._

    assertEquals(
      Just(1).traverse(x => Writer("foo" -> (x + 5))).run(),
      "foo" -> Just(6)
    )

    assertEquals(
      Nothing[Int].traverse(x => Writer("foo" -> (x + 5))).run(),
      "" -> Nothing[Int]
    )

    assertEquals(
      Just(1).traverse(x => Writer(List("foo") -> (x + 5))).run(),
      List("foo") -> Just(6)
    )

    assertEquals(
      Nothing[Int].traverse(x => Writer(List("foo") -> (x + 5))).run(),
      Nil -> Nothing[Int]
    )
  }

  property("Sequence Maybe[Writer] syntax") {
    import tech.backwards.fp.learn.Traversal.syntax._

    assertEquals(
      Just(Writer("foo" -> 1)).sequence.run(),
      "foo" -> Just(1)
    )

    assertEquals(
      Nothing[Writer[String, Int]].sequence.run(),
      "" -> Nothing[Int]
    )

    assertEquals(
      Just(Writer(List("foo") -> 1)).sequence.run(),
      List("foo") -> Just(1)
    )

    assertEquals(
      Nothing[Writer[List[String], Int]].sequence.run(),
      Nil -> Nothing[Int]
    )
  }

  property("Traverse Disjunction[Writer]") {
    assertEquals(
      Traversal[Disjunction[String, *]].traverse(Right(1))(x => Writer("foo" -> (x + 5))).run(),
      "foo" -> Right(6)
    )

    assertEquals(
      Traversal[Disjunction[String, *]].traverse(Left[String, Int]("a"))(x => Writer("foo" -> (x + 5))).run(),
      "" -> Left("a")
    )

    assertEquals(
      Traversal[Disjunction[String, *]].traverse(Right(1))(x => Writer(List("foo") -> (x + 5))).run(),
      List("foo") -> Right(6)
    )

    assertEquals(
      Traversal[Disjunction[String, *]].traverse(Left[String, Int]("a"))(x => Writer(List("foo") -> (x + 5))).run(),
      Nil -> Left("a")
    )
  }

  property("Traverse Disjunction[Writer] syntax") {
    import tech.backwards.fp.learn.Traversal.syntax._

    assertEquals(
      Right(1).traverse(x => Writer("foo" -> (x + 5))).run(),
      "foo" -> Right(6)
    )

    assertEquals(
      Left[String, Int]("a").traverse(x => Writer("foo" -> (x + 5))).run(),
      "" -> Left("a")
    )

    assertEquals(
      Right(1).traverse(x => Writer(List("foo") -> (x + 5))).run(),
      List("foo") -> Right(6)
    )

    assertEquals(
      Left[String, Int]("a").traverse(x => Writer(List("foo") -> (x + 5))).run(),
      Nil -> Left("a")
    )
  }

  property("Sequence Disjunction[Writer] syntax") {
    import tech.backwards.fp.learn.Traversal.syntax._

    assertEquals(
      Right(Writer("foo" -> 1)).sequence.run(),
      "foo" -> Right(1)
    )

    assertEquals(
      Left[String, Writer[String, Int]]("a").sequence.run(),
      "" -> Left("a")
    )

    assertEquals(
      Right(Writer(List("foo") -> 1)).sequence.run(),
      List("foo") -> Right(1)
    )

    assertEquals(
      Left[String, Writer[List[String], Int]]("a").sequence.run(),
      Nil -> Left("a")
    )
  }

  test("Traverse Writer[Id]") {
    assertEquals(
      Traversal[Writer[String, *]].traverse(Writer("foo" -> 1))(x => Id(x)).value.run(),
      "foo" -> 1
    )

    assertEquals(
      Traversal[Writer[List[String], *]].traverse(Writer(List("foo") -> 1))(x => Id(x)).value.run(),
      List("foo") -> 1
    )
  }

  property("Traverse Writer[Tuple2]") {
    val (fst: Writer[String, Int], snd: Writer[String, Int]) =
      Traversal[Writer[String, *]].traverse[Lambda[X => (X, X)], Int, Int](Writer("foo" -> 1))(x => (x + 1, x + 2))

    assertEquals(
      fst.run(),
      "foo" -> 2
    )

    assertEquals(
      snd.run(),
      "foo" -> 3
    )
  }

  property("Traverse Writer[Tuple2] syntax") {
    import tech.backwards.fp.learn.Traversal.syntax._

    val (fst: Writer[String, Int], snd: Writer[String, Int]) =
      Writer("foo" -> 1).traverse[Lambda[X => (X, X)], Int](x => (x + 1, x + 2))

    assertEquals(
      fst.run(),
      "foo" -> 2
    )

    assertEquals(
      snd.run(),
      "foo" -> 3
    )
  }

  property("Sequence Writer[Tuple2] syntax") {
    import tech.backwards.fp.learn.Traversal.syntax._

    val (fst: Writer[String, Int], snd: Writer[String, Int]) =
      Writer("foo" -> (1, 2)).sequence

    assertEquals(
      fst.run(),
      "foo" -> 1
    )

    assertEquals(
      snd.run(),
      "foo" -> 2
    )
  }

  property("Traverse Writer[List]") {
    assertEquals(
      Traversal[Writer[String, *]].traverse(Writer("foo" -> 1))(x => List(x + 1, x + 2)).map(_.run()),
      List("foo" -> 2, "foo" -> 3)
    )

    assertEquals(
      Traversal[Writer[String, *]].traverse(Writer("foo" -> 1))(_ => Nil: List[Int]).map(_.run()),
      Nil
    )
  }

  property("Traverse Writer[List] syntax") {
    import tech.backwards.fp.learn.Traversal.syntax._

    assertEquals(
      Writer("foo" -> 1).traverse(x => List(x + 1, x + 2)).map(_.run()),
      List("foo" -> 2, "foo" -> 3)
    )

    assertEquals(
      Writer("foo" -> 1).traverse(_ => Nil: List[Int]).map(_.run()),
      Nil
    )
  }

  property("Sequence Writer[List] syntax") {
    import tech.backwards.fp.learn.Traversal.syntax._

    assertEquals(
      Writer("foo" -> List(1, 2)).sequence.map(_.run()),
      List("foo" -> 1, "foo" -> 2)
    )

    assertEquals(
      Writer("foo" -> List.empty[Int]).sequence.map(_.run()),
      Nil
    )
  }

  property("Traverse Writer[Maybe]") {
    import tech.backwards.fp.learn.Functor.syntax._

    assertEquals(
      Traversal[Writer[String, *]].traverse(Writer("foo" -> 1))(x => Just(x + 1)).map(_.run()),
      Just("foo" -> 2)
    )

    assertEquals(
      Traversal[Writer[String, *]].traverse(Writer("foo" -> 1))(_ => Nothing[Int]).map(_.run()),
      Nothing[(String, Int)]
    )

    assertEquals(
      Traversal[Writer[String, *]].traverse(Writer("foo" -> 1))(_ => Nothing[Int]),
      Nothing[Writer[String, Int]]
    )
  }

  property("Traverse Writer[Maybe] syntax") {
    import tech.backwards.fp.learn.Functor.syntax._
    import tech.backwards.fp.learn.Traversal.syntax._

    assertEquals(
      Writer("foo" -> 1).traverse(x => Just(x + 1)).map(_.run()),
      Just("foo" -> 2)
    )

    assertEquals(
      Writer("foo" -> 1).traverse(_ => Nothing[Int]).map(_.run()),
      Nothing[(String, Int)]
    )

    assertEquals(
      Writer("foo" -> 1).traverse(_ => Nothing[Int]),
      Nothing[Writer[String, Int]]
    )
  }

  property("Sequence Writer[Maybe] syntax") {
    import tech.backwards.fp.learn.Functor.syntax._
    import tech.backwards.fp.learn.Traversal.syntax._

    assertEquals(
      Writer("foo" -> Just(1)).sequence.map(_.run()),
      Just("foo" -> 1)
    )

    assertEquals(
      Writer("foo" -> Nothing[Int]).sequence.map(_.run()),
      Nothing[(String, Int)]
    )

    assertEquals(
      Writer("foo" -> Nothing[Int]).sequence,
      Nothing[Writer[String, Int]]
    )
  }

  property("Traverse Writer[Disjunction]") {
    import tech.backwards.fp.learn.Functor.syntax._

    assertEquals(
      Traversal[Writer[String, *]].traverse(Writer("foo" -> 1))(x => Right(x + 1)).map(_.run()),
      Right("foo" -> 2)
    )

    assertEquals(
      Traversal[Writer[String, *]].traverse(Writer("foo" -> 1))(_ => Left[String, Int]("whoops")).map(_.run()),
      Left("whoops")
    )

    assertEquals(
      Traversal[Writer[String, *]].traverse(Writer("foo" -> 1))(_ => Left[String, Int]("whoops")),
      Left("whoops")
    )
  }

  property("Traverse Writer[Disjunction] syntax") {
    import tech.backwards.fp.learn.Functor.syntax._
    import tech.backwards.fp.learn.Traversal.syntax._

    assertEquals(
      Writer("foo" -> 1).traverse(x => Right(x + 1)).map(_.run()),
      Right("foo" -> 2)
    )

    assertEquals(
      Writer("foo" -> 1).traverse(_ => Left[String, Int]("whoops")).map(_.run()),
      Left("whoops")
    )

    assertEquals(
      Writer("foo" -> 1).traverse(_ => Left[String, Int]("whoops")),
      Left("whoops")
    )
  }

  property("Sequence Writer[Disjunction] syntax") {
    import tech.backwards.fp.learn.Functor.syntax._
    import tech.backwards.fp.learn.Traversal.syntax._

    assertEquals(
      Writer("foo" -> Right(1)).sequence.map(_.run()),
      Right("foo" -> 1)
    )

    assertEquals(
      Writer("foo" -> Left[String, Int]("whoops")).sequence.map(_.run()),
      Left("whoops")
    )

    assertEquals(
      Writer("foo" -> Left[String, Int]("whoops")).sequence,
      Left("whoops")
    )
  }

  property("Traverse Writer[State]") {
    val state: State[String, Writer[String, Int]] =
      Traversal[Writer[String, *]].traverse(Writer("foo" -> 1))(x => State((s: String) => s -> (x + 1)))

    val (s: String, writer: Writer[String, Int]) =
      state.run("bar")

    assertEquals(s, "bar")

    val (ss: String, i: Int) =
      writer.run()

    assertEquals(ss, "foo")
    assertEquals(i, 2)
  }

  property("Traverse Writer[State] syntax") {
    import tech.backwards.fp.learn.Traversal.syntax._

    val state: State[String, Writer[String, Int]] =
      Writer("foo" -> 1).traverse(x => State((s: String) => s -> (x + 1)))

    val (s: String, writer: Writer[String, Int]) =
      state.run("bar")

    assertEquals(s, "bar")

    val (ss: String, i: Int) =
      writer.run()

    assertEquals(ss, "foo")
    assertEquals(i, 2)
  }

  property("Sequence Writer[State] syntax") {
    import tech.backwards.fp.learn.Traversal.syntax._

    val state: State[String, Writer[String, Int]] =
      Writer("foo" -> State((s: String) => s -> 1)).sequence

    val (s: String, writer: Writer[String, Int]) =
      state.run("bar")

    assertEquals(s, "bar")

    val (ss: String, i: Int) =
      writer.run()

    assertEquals(ss, "foo")
    assertEquals(i, 1)
  }
}