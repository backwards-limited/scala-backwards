package tech.backwards.fp.learn

import munit.ScalaCheckSuite
import org.scalacheck.Test

/**
 * Note that Monads cannot be nested - Instead we use Monad Transformers
 */
class NestedSuite extends ScalaCheckSuite {
  override def scalaCheckTestParameters: Test.Parameters =
    super.scalaCheckTestParameters.withMinSuccessfulTests(100).withMaxDiscardRatio(10)

  property("Nested Id/Id Functor") {
    val nested: Nested[Id, Id, Int] =
      Functor[Nested[Id, Id, *]].fmap(Nested(Id(Id(5))))(_ + 1)

    assertEquals(
      nested,
      Nested(Id(Id(6)))
    )

    assertEquals(
      Functor[Nested[Id, Id, *]].fmap(Nested(Id(Id("foo"))))(_ + "bar"),
      Nested(Id(Id("foobar")))
    )
  }

  property("Nested Id/Id Functor syntax") {
    import tech.backwards.fp.learn.Functor.syntax._

    assertEquals(
      Nested(Id(Id("foo"))).fmap(_ + "bar").value,
      Id(Id("foobar"))
    )
  }

  property("Nested Id/Maybe Functor") {
    val nested: Nested[Id, Maybe, Int] =
      Functor[Nested[Id, Maybe, *]].fmap(Nested(Id(Just(5))))(_ + 1)

    assertEquals(
      nested,
      Nested(Id(Just(6)))
    )

    assertEquals(
      Functor[Nested[Id, Maybe, *]].fmap(Nested(Id(Just("foo"))))(_ + "bar"),
      Nested(Id(Just("foobar")))
    )

    assertEquals(
      Functor[Nested[Id, Maybe, *]].fmap(Nested(Id(Nothing[Int])))(_ + 1),
      Nested(Id(Nothing[Int]))
    )
  }

  property("Nested Id/Maybe Functor syntax") {
    import tech.backwards.fp.learn.Functor.syntax._
    import tech.backwards.fp.learn.Maybe.syntax._

    assertEquals(
      Nested(Id("foo".just)).fmap(_ + "bar").value,
      Id("foobar".just)
    )

    assertEquals(
      Nested(Id(Nothing[Int])).fmap(_ + 1).value,
      Id(Nothing[Int])
    )
  }

  property("Nested List/Maybe Functor") {
    val nested: Nested[List, Maybe, Int] =
      Functor[Nested[List, Maybe, *]].fmap(Nested(List(Just(1), Just(2), Just(3))))(_ + 1)

    assertEquals(
      nested,
      Nested(List(Just(2), Just(3), Just(4)))
    )

    assertEquals(
      Functor[Nested[List, Maybe, *]].fmap(Nested(List(Just(1), Just(2), Just(3))))(_ + 1),
      Nested(List(Just(2), Just(3), Just(4)))
    )

    assertEquals(
      Functor[Nested[List, Maybe, *]].fmap(Nested(List(Just(1), Nothing[Int], Just(3))))(_ + 1),
      Nested(List(Just(2), Nothing[Int], Just(4)))
    )
  }

  property("Nested List/Maybe Functor syntax") {
    import tech.backwards.fp.learn.Functor.syntax._
    import tech.backwards.fp.learn.Maybe.syntax._

    assertEquals(
      Nested(List(1.just, 2.just, 3.just)).fmap(_ + 1).value,
      List(2.just, 3.just, 4.just)
    )

    assertEquals(
      Nested(List(Just(1), Nothing[Int], Just(3))).fmap(_ + 1).value,
      List(Just(2), Nothing[Int], Just(4))
    )
  }

  property("Nested Id/Id Applicative") {
    val nested: Nested[Id, Id, Int] =
      Applicative[Nested[Id, Id, *]].ap(Nested[Id, Id, Int => Int](Id(Id(_ + 1))))(Nested(Id(Id(5))))

    assertEquals(
      nested,
      Nested(Id(Id(6)))
    )

    assertEquals(
      Applicative[Nested[Id, Id, *]].ap(Nested[Id, Id, Int => String](Id(Id(_.toString))))(Nested(Id(Id(5)))),
      Nested(Id(Id("5")))
    )
  }

  property("Nested Id/Id Applicative syntax") {
    import tech.backwards.fp.learn.Applicative.syntax.function._

    assertEquals(
      Nested(Id(Id((_: Int) + 1))).ap(Nested(Id(Id(5)))).value,
      Id(Id(6))
    )
  }

  property("Nested Id/Id Applicative and Functor syntax") {
    import tech.backwards.fp.learn.Applicative.syntax.function._
    import tech.backwards.fp.learn.Functor.syntax._

    assertEquals(
      Nested(Id(Id(5))).fmap((x: Int) => (y: Int) => x + y).ap(Nested(Id(Id(6)))).value,
      Id(Id(11))
    )
  }

  property("Nested Id/Id Applicative and Functor function syntax") {
    import tech.backwards.fp.learn.Applicative.syntax.function._
    import tech.backwards.fp.learn.Functor.syntax.function._

    assertEquals(
      ((x: Int) => (y: Int) => x + y) `<$>` Nested(Id(Id(5))) <*> Nested(Id(Id(6))),
      Nested(Id(Id(11)))
    )

    val add: Int => Int => Int =
      x => y => x + y

    assertEquals(
      add `<$>` Nested(Id(Id(5))) <*> Nested(Id(Id(6))),
      Nested(Id(Id(11)))
    )
  }

  property("Nested Id/Maybe Applicative") {
    val nested: Nested[Id, Maybe, Int] =
      Applicative[Nested[Id, Maybe, *]].ap(Nested(Id(Just((_: Int) + 1))))(Nested(Id(Just(5))))

    assertEquals(
      nested,
      Nested(Id(Just(6)))
    )

    assertEquals(
      Applicative[Nested[Id, Maybe, *]].ap(Nested(Id(Just((_: Int).toString))))(Nested(Id(Just(5)))),
      Nested(Id(Just("5")))
    )

    assertEquals(
      Applicative[Nested[Id, Maybe, *]].ap(Nested(Id(Just((_: Int).toString))))(Nested(Id(Nothing[Int]))),
      Nested(Id(Nothing[String]))
    )

    assertEquals(
      Applicative[Nested[Id, Maybe, *]].ap(Nested(Id(Nothing[Int => String])))(Nested(Id(Just(5)))),
      Nested(Id(Nothing[String]))
    )
  }

  property("Nested Id/Maybe Applicative syntax") {
    import tech.backwards.fp.learn.Applicative.syntax.function._
    import tech.backwards.fp.learn.Maybe.syntax._

    assertEquals(
      Nested(Id(((x: Int) => x.toString).just)) <*> Nested(Id(5.just)),
      Nested(Id("5".just))
    )

    assertEquals(
      Nested(Id(((x: Int) => x.toString).just)) <*> Nested(Id(Nothing[Int])),
      Nested(Id(Nothing[String]))
    )

    assertEquals(
      Nested(Id(Nothing[Int => String])) <*> Nested(Id(5.just)),
      Nested(Id(Nothing[String]))
    )
  }

  property("Nested Id/Maybe Applicative and Functor syntax") {
    import tech.backwards.fp.learn.Applicative.syntax.function._
    import tech.backwards.fp.learn.Functor.syntax._
    import tech.backwards.fp.learn.Maybe.syntax._

    assertEquals(
      Nested(Id(5.just)).fmap((x: Int) => (y: Int) => x + y).ap(Nested(Id(6.just))).value,
      Id(11.just)
    )

    assertEquals(
      Nested(Id(5.just)).fmap((x: Int) => (y: Int) => x + y).ap(Nested(Id(Nothing[Int]))).value,
      Id(Nothing[Int])
    )

    assertEquals(
      Nested(Id(Nothing[Int])).fmap((x: Int) => (y: Int) => x + y).ap(Nested(Id(6.just))).value,
      Id(Nothing[Int])
    )
  }

  property("Nested Id/Maybe Applicative and Functor function syntax") {
    import tech.backwards.fp.learn.Applicative.syntax.function._
    import tech.backwards.fp.learn.Functor.syntax.function._
    import tech.backwards.fp.learn.Maybe.syntax._

    assertEquals(
      ((x: Int) => (y: Int) => x + y) `<$>` Nested(Id(5.just)) <*> Nested(Id(6.just)),
      Nested(Id(11.just))
    )

    assertEquals(
      ((x: Int) => (y: Int) => x + y) `<$>` Nested(Id(5.just)) <*> Nested(Id(Nothing[Int])),
      Nested(Id(Nothing[Int]))
    )

    assertEquals(
      ((x: Int) => (y: Int) => x + y) `<$>` Nested(Id(Nothing[Int])) <*> Nested(Id(6.just)),
      Nested(Id(Nothing[Int]))
    )

    val add: Int => Int => Int =
      x => y => x + y

    assertEquals(
      add `<$>` Nested(Id(5.just)) <*> Nested(Id(6.just)),
      Nested(Id(11.just))
    )

    assertEquals(
      add `<$>` Nested(Id(5.just)) <*> Nested(Id(Nothing[Int])),
      Nested(Id(Nothing[Int]))
    )

    assertEquals(
      add `<$>` Nested(Id(Nothing[Int])) <*> Nested(Id(6.just)),
      Nested(Id(Nothing[Int]))
    )
  }

  property("Nested Id/Disjunction Applicative") {
    val nested: Nested[Id, Disjunction[String, *], Int] =
      Applicative[Nested[Id, Disjunction[String, *], *]].ap(Nested(Id(Right[String, Int => Int](_ + 1))))(Nested(Id(Right(5))))

    assertEquals(
      nested,
      Nested(Id(Right[String, Int](6)))
    )

    assertEquals(
      Applicative[Nested[Id, Disjunction[String, *], *]].ap(Nested(Id(Right[String, Int => Int](_ + 1))))(Nested(Id(Right(5)))),
      Nested(Id(Right[String, Int](6)))
    )

    assertEquals(
      Applicative[Nested[Id, Disjunction[String, *], *]].ap(Nested(Id(Right[String, Int => Int](_ + 1))))(Nested(Id(Left("whoops")))),
      Nested(Id(Left[String, Int]("whoops")))
    )

    assertEquals(
      Applicative[Nested[Id, Disjunction[String, *], *]].ap(Nested(Id(Left[String, Int => Int]("whoops"))))(Nested(Id(Right(5)))),
      Nested(Id(Left[String, Int]("whoops")))
    )
  }

  property("Nested Id/Disjunction Applicative syntax") {
    import tech.backwards.fp.learn.Applicative.syntax.function._
    import tech.backwards.fp.learn.Disjunction.syntax._

    assertEquals(
      Nested(Id(((_: Int) + 1).right[String])) <*> Nested(Id(5.right)),
      Nested(Id(6.right[String]))
    )

    assertEquals(
      Nested(Id(((_: Int) + 1).right[String])) <*> Nested(Id("whoops".left)),
      Nested(Id("whoops".left[Int]))
    )

    assertEquals(
      Nested(Id("whoops".left[Int => Int])) <*> Nested(Id(5.right)),
      Nested(Id("whoops".left[Int]))
    )
  }

  property("Nested Id/Disjunction Applicative and Functor syntax") {
    import tech.backwards.fp.learn.Applicative.syntax.function._
    import tech.backwards.fp.learn.Disjunction.syntax._
    import tech.backwards.fp.learn.Functor.syntax._

    assertEquals(
      Nested(Id(5.right)).fmap((x: Int) => (y: Int) => x + y).ap(Nested(Id(6.right))).value,
      Id(11.right)
    )

    assertEquals(
      Nested(Id(5.right[String])).fmap((x: Int) => (y: Int) => x + y).ap(Nested(Id("whoops".left))).value,
      Id("whoops".left[Int])
    )

    assertEquals(
      Nested(Id("whoops".left[Int])).fmap((x: Int) => (y: Int) => x + y).ap(Nested(Id(6.right))).value,
      Id("whoops".left[Int])
    )
  }

  property("Nested Id/Disjunction Applicative and Functor function syntax") {
    import tech.backwards.fp.learn.Applicative.syntax.function._
    import tech.backwards.fp.learn.Disjunction.syntax._
    import tech.backwards.fp.learn.Functor.syntax.function._

    assertEquals(
      ((x: Int) => (y: Int) => x + y) `<$>` Nested(Id(5.right)) <*> Nested(Id(6.right)),
      Nested(Id(11.right))
    )

    assertEquals(
      ((x: Int) => (y: Int) => x + y) `<$>` Nested(Id(5.right[String])) <*> Nested(Id("whoops".left)),
      Nested(Id("whoops".left[Int]))
    )

    assertEquals(
      ((x: Int) => (y: Int) => x + y) `<$>` Nested(Id("whoops".left[Int])) <*> Nested(Id(6.right)),
      Nested(Id("whoops".left[Int]))
    )

    val add: Int => Int => Int =
      x => y => x + y

    assertEquals(
      add `<$>` Nested(Id(5.right)) <*> Nested(Id(6.right)),
      Nested(Id(11.right))
    )

    assertEquals(
      add `<$>` Nested(Id(5.right[String])) <*> Nested(Id("whoops".left)),
      Nested(Id("whoops".left[Int]))
    )

    assertEquals(
      add `<$>` Nested(Id("whoops".left[Int])) <*> Nested(Id(6.right)),
      Nested(Id("whoops".left[Int]))
    )
  }

  property("Nested List/Maybe Applicative") {
    val nested: Nested[List, Maybe, Int] =
      Applicative[Nested[List, Maybe, *]].ap(Nested(List(Just((_: Int) + 1), Just((_: Int) + 3))))(Nested(List(Just(5), Just(6))))

    assertEquals(
      nested,
      Nested(List(Just(6), Just(7), Just(8), Just(9)))
    )

    assertEquals(
      Applicative[Nested[List, Maybe, *]].ap(Nested(List(Just((_: Int) + 1), Just((_: Int) + 3))))(Nested(List(Just(5), Just(6)))),
      Nested(List(Just(6), Just(7), Just(8), Just(9)))
    )

    assertEquals(
      Applicative[Nested[List, Maybe, *]].ap(Nested(List(Just((_: Int) + 1), Just((_: Int) + 3))))(Nested(List(Just(5), Nothing[Int]))),
      Nested(List(Just(6), Nothing[Int], Just(8), Nothing[Int]))
    )

    assertEquals(
      Applicative[Nested[List, Maybe, *]].ap(Nested(List(Just((_: Int) + 1), Nothing[Int => Int])))(Nested(List(Just(5), Just(6)))),
      Nested(List(Just(6), Just(7), Nothing[Int], Nothing[Int]))
    )
  }

  property("Nested List/Maybe Applicative syntax") {
    import tech.backwards.fp.learn.Applicative.syntax.function._
    import tech.backwards.fp.learn.Maybe.syntax._

    assertEquals(
      Nested(List(((_: Int) + 1).just, ((_: Int) + 3).just)) <*> Nested(List(5.just, 6.just)),
      Nested(List(6.just, 7.just, 8.just, 9.just))
    )

    assertEquals(
      Nested(List(((_: Int) + 1).just, ((_: Int) + 3).just)) <*> Nested(List(5.just, Nothing[Int])),
      Nested(List(6.just, Nothing[Int], 8.just, Nothing[Int]))
    )

    assertEquals(
      Nested(List(((_: Int) + 1).just, Nothing[Int => Int])) <*> Nested(List(5.just, 6.just)),
      Nested(List(6.just, 7.just, Nothing[Int], Nothing[Int]))
    )
  }

  property("Nested List/Maybe Applicative and Functor syntax") {
    import tech.backwards.fp.learn.Applicative.syntax.function._
    import tech.backwards.fp.learn.Functor.syntax._
    import tech.backwards.fp.learn.Maybe.syntax._

    assertEquals(
      Nested(List(1.just, 2.just, 3.just)).fmap((x: Int) => (y: Int) => x + y).ap(Nested(List(5.just, 6.just))).value,
      List(6.just, 7.just, 7.just, 8.just, 8.just, 9.just)
    )

    assertEquals(
      Nested(List(1.just, 2.just, 3.just)).fmap((x: Int) => (y: Int) => x + y).ap(Nested(List(5.just, Nothing[Int]))).value,
      List(6.just, Nothing[Int], 7.just, Nothing[Int], 8.just, Nothing[Int])
    )

    assertEquals(
      Nested(List(1.just, Nothing[Int], 3.just)).fmap((x: Int) => (y: Int) => x + y).ap(Nested(List(5.just, 6.just))).value,
      List(6.just, 7.just, Nothing[Int], Nothing[Int], 8.just, 9.just)
    )
  }

  property("Nested List/Maybe Applicative and Functor function syntax") {
    import tech.backwards.fp.learn.Applicative.syntax.function._
    import tech.backwards.fp.learn.Functor.syntax.function._
    import tech.backwards.fp.learn.Maybe.syntax._

    assertEquals(
      ((x: Int) => (y: Int) => x + y) `<$>` Nested(List(1.just, 2.just, 3.just)) <*> Nested(List(5.just, 6.just)),
      Nested(List(6.just, 7.just, 7.just, 8.just, 8.just, 9.just))
    )

    assertEquals(
      ((x: Int) => (y: Int) => x + y) `<$>` Nested(List(1.just, 2.just, 3.just)) <*> Nested(List(5.just, Nothing[Int])),
      Nested(List(6.just, Nothing[Int], 7.just, Nothing[Int], 8.just, Nothing[Int]))
    )

    assertEquals(
      ((x: Int) => (y: Int) => x + y) `<$>` Nested(List(1.just, Nothing[Int], 3.just)) <*> Nested(List(5.just, 6.just)),
      Nested(List(6.just, 7.just, Nothing[Int], Nothing[Int], 8.just, 9.just))
    )

    val add: Int => Int => Int =
      x => y => x + y

    assertEquals(
      add `<$>` Nested(List(1.just, 2.just, 3.just)) <*> Nested(List(5.just, 6.just)),
      Nested(List(6.just, 7.just, 7.just, 8.just, 8.just, 9.just))
    )

    assertEquals(
      add `<$>` Nested(List(1.just, 2.just, 3.just)) <*> Nested(List(5.just, Nothing[Int])),
      Nested(List(6.just, Nothing[Int], 7.just, Nothing[Int], 8.just, Nothing[Int]))
    )

    assertEquals(
      add `<$>` Nested(List(1.just, Nothing[Int], 3.just)) <*> Nested(List(5.just, 6.just)),
      Nested(List(6.just, 7.just, Nothing[Int], Nothing[Int], 8.just, 9.just))
    )
  }

  property("Nested List/Disjunction Applicative") {
    val nested: Nested[List, Disjunction[String, *], Int] =
      Applicative[Nested[List, Disjunction[String, *], *]].ap(Nested(List(Right((_: Int) + 1), Right((_: Int) + 3))))(Nested(List(Right(5), Right(6))))

    assertEquals(
      nested,
      Nested(List(Right[String, Int](6), Right(7), Right(8), Right(9)))
    )

    assertEquals(
      Applicative[Nested[List, Disjunction[String, *], *]].ap(Nested(List(Right((_: Int) + 1), Right((_: Int) + 3))))(Nested(List(Right(5), Right(6)))),
      Nested(List(Right[String, Int](6), Right(7), Right(8), Right(9)))
    )

    assertEquals(
      Applicative[Nested[List, Disjunction[String, *], *]].ap(Nested(List(Right((_: Int) + 1), Right((_: Int) + 3))))(Nested(List(Right(5), Left("whoops")))),
      Nested(List(Right(6), Left("whoops"), Right(8), Left("whoops")))
    )

    assertEquals(
      Applicative[Nested[List, Disjunction[String, *], *]].ap(Nested(List(Right((_: Int) + 1), Left("whoops"))))(Nested(List(Right(5), Right(6)))),
      Nested(List(Right(6), Right(7), Left("whoops"), Left("whoops")))
    )
  }

  property("Nested List/Disjunction Applicative syntax") {
    import tech.backwards.fp.learn.Applicative.syntax.function._
    import tech.backwards.fp.learn.Disjunction.syntax._

    assertEquals(
      Nested(List(((_: Int) + 1).right[String], ((_: Int) + 3).right)) <*> Nested(List(5.right, 6.right)),
      Nested(List(6.right[String], 7.right, 8.right, 9.right))
    )

    assertEquals(
      Nested(List(((_: Int) + 1).right[String], ((_: Int) + 3).right)) <*> Nested(List(5.right, "whoops".left)),
      Nested(List(6.right, "whoops".left, 8.right, "whoops".left))
    )

    assertEquals(
      Nested(List(((_: Int) + 1).right[String], "whoops".left)).ap(Nested(List(5.right, 6.right))),
      Nested(List(6.right, 7.right, "whoops".left, "whoops".left))
    )
  }

  property("Nested List/Disjunction Applicative and Functor syntax") {
    import tech.backwards.fp.learn.Applicative.syntax.function._
    import tech.backwards.fp.learn.Disjunction.syntax._
    import tech.backwards.fp.learn.Functor.syntax._

    assertEquals(
      Nested(List(1.right, 2.right, 3.right)).fmap((x: Int) => (y: Int) => x + y).ap(Nested(List(5.right, 6.right))).value,
      List(6.right, 7.right, 7.right, 8.right, 8.right, 9.right)
    )

    assertEquals(
      Nested(List(1.right[String], 2.right, 3.right)).fmap((x: Int) => (y: Int) => x + y).ap(Nested(List(5.right, "whoops".left))).value,
      List(6.right, "whoops".left, 7.right, "whoops".left, 8.right, "whoops".left)
    )

    assertEquals(
      Nested(List(1.right, "whoops".left, 3.right)).fmap((x: Int) => (y: Int) => x + y).ap(Nested(List(5.right, 6.right))).value,
      List(6.right, 7.right, "whoops".left, "whoops".left, 8.right, 9.right)
    )
  }

  property("Nested List/Disjunction Applicative and Functor function syntax") {
    import tech.backwards.fp.learn.Applicative.syntax.function._
    import tech.backwards.fp.learn.Disjunction.syntax._
    import tech.backwards.fp.learn.Functor.syntax.function._

    assertEquals(
      ((x: Int) => (y: Int) => x + y) `<$>` Nested(List(1.right, 2.right, 3.right)) <*> Nested(List(5.right, 6.right)),
      Nested(List(6.right, 7.right, 7.right, 8.right, 8.right, 9.right))
    )

    assertEquals(
      ((x: Int) => (y: Int) => x + y) `<$>` Nested(List(1.right[String], 2.right, 3.right)) <*> Nested(List(5.right, "whoops".left)),
      Nested(List(6.right, "whoops".left, 7.right, "whoops".left, 8.right, "whoops".left))
    )

    assertEquals(
      ((x: Int) => (y: Int) => x + y) `<$>` Nested(List(1.right, "whoops".left, 3.right)) <*> Nested(List(5.right, 6.right)),
      Nested(List(6.right, 7.right, "whoops".left, "whoops".left, 8.right, 9.right))
    )

    val add: Int => Int => Int =
      x => y => x + y

    assertEquals(
      add `<$>` Nested(List(1.right, 2.right, 3.right)) <*> Nested(List(5.right, 6.right)),
      Nested(List(6.right, 7.right, 7.right, 8.right, 8.right, 9.right))
    )

    assertEquals(
      add `<$>` Nested(List(1.right[String], 2.right, 3.right)) <*> Nested(List(5.right, "whoops".left)),
      Nested(List(6.right, "whoops".left, 7.right, "whoops".left, 8.right, "whoops".left))
    )

    assertEquals(
      add `<$>` Nested(List(1.right, "whoops".left, 3.right)) <*> Nested(List(5.right, 6.right)),
      Nested(List(6.right, 7.right, "whoops".left, "whoops".left, 8.right, 9.right))
    )
  }

  property("Traverse Id[Nested[Id[Id]]]") {
    val nested: Nested[Id, Id, Id[Int]] =
      Traversal[Id].traverse(Id(5))(x => Nested(Id(Id(x + 1))))

    assertEquals(
      nested,
      Nested(Id(Id(Id(6))))
    )

    assertEquals(
      Traversal[Id].traverse(Id(5))(x => Nested(Id(Id(x + 1)))),
      Nested(Id(Id(Id(6))))
    )
  }

  property("Traverse Id[Nested[Id[Id]]] syntax") {
    import tech.backwards.fp.learn.Traversal.syntax._

    assertEquals(
      Id(5).traverse(x => Nested(Id(Id(x + 1)))),
      Nested(Id(Id(Id(6))))
    )
  }

  property("Sequence Id[Nested[Id[Id]]] syntax") {
    import tech.backwards.fp.learn.Traversal.syntax._

    assertEquals(
      Id(Nested(Id(Id(5)))).sequence,
      Nested(Id(Id(Id(5))))
    )
  }

  property("Traverse List[Nested[Id[Id]]]") {
    val nested: Nested[Id, Id, List[Int]] =
      Traversal[List].traverse(List(1, 2))(x => Nested(Id(Id(x + 1))))

    assertEquals(
      nested,
      Nested(Id(Id(List(2, 3))))
    )

    assertEquals(
      Traversal[List].traverse(List(1, 2))(x => Nested(Id(Id(x + 1)))),
      Nested(Id(Id(List(2, 3))))
    )
  }

  property("Traverse List[Nested[Id[Id]]] syntax") {
    import tech.backwards.fp.learn.Traversal.syntax._

    assertEquals(
      List(1, 2).traverse(x => Nested(Id(Id(x + 1)))),
      Nested(Id(Id(List(2, 3))))
    )
  }

  property("Sequence List[Nested[Id[Id]]] syntax") {
    import tech.backwards.fp.learn.Traversal.syntax._

    assertEquals(
      List(Nested(Id(Id(1))), Nested(Id(Id(2)))).sequence,
      Nested(Id(Id(List(1, 2))))
    )
  }
}