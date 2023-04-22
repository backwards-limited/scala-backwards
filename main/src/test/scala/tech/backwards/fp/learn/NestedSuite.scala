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

    assertEquals(
      Nested(Id(Just("foo"))).fmap(_ + "bar").value,
      Id(Just("foobar"))
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

    assertEquals(
      Nested(List(Just(1), Just(2), Just(3))).fmap(_ + 1).value,
      List(Just(2), Just(3), Just(4))
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
    import tech.backwards.fp.learn.Functor.syntax._
    import tech.backwards.fp.learn.Applicative.syntax.function._

    assertEquals(
      Nested(Id(Id(5))).fmap((x: Int) => (y: Int) => x + y).ap(Nested(Id(Id(6)))).value,
      Id(Id(11))
    )
  }

  property("Nested Id/Id Applicative and Functor function syntax") {
    import tech.backwards.fp.learn.Functor.syntax.function._
    import tech.backwards.fp.learn.Applicative.syntax.function._

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

    assertEquals(
      Nested(Id(Just((_: Int).toString))) ap Nested(Id(Just(5))),
      Nested(Id(Just("5")))
    )

    assertEquals(
      Nested(Id(Just((_: Int).toString))) ap Nested(Id(Nothing[Int])),
      Nested(Id(Nothing[String]))
    )

    assertEquals(
      Nested(Id(Nothing[Int => String])) ap Nested(Id(Just(5))),
      Nested(Id(Nothing[String]))
    )
  }

  property("Nested Id/Maybe Applicative and Functor syntax") {
    import tech.backwards.fp.learn.Functor.syntax._
    import tech.backwards.fp.learn.Applicative.syntax.function._

    assertEquals(
      Nested(Id(Just(5))).fmap((x: Int) => (y: Int) => x + y).ap(Nested(Id(Just(6)))).value,
      Id(Just(11))
    )

    assertEquals(
      Nested(Id(Just(5))).fmap((x: Int) => (y: Int) => x + y).ap(Nested(Id(Nothing[Int]))).value,
      Id(Nothing[Int])
    )

    assertEquals(
      Nested(Id(Nothing[Int])).fmap((x: Int) => (y: Int) => x + y).ap(Nested(Id(Just(6)))).value,
      Id(Nothing[Int])
    )
  }

  property("Nested Id/Maybe Applicative and Functor function syntax") {
    import tech.backwards.fp.learn.Functor.syntax.function._
    import tech.backwards.fp.learn.Applicative.syntax.function._

    assertEquals(
      ((x: Int) => (y: Int) => x + y) `<$>` Nested(Id(Just(5))) <*> Nested(Id(Just(6))),
      Nested(Id(Just(11)))
    )

    assertEquals(
      ((x: Int) => (y: Int) => x + y) `<$>` Nested(Id(Just(5))) <*> Nested(Id(Nothing[Int])),
      Nested(Id(Nothing[Int]))
    )

    assertEquals(
      ((x: Int) => (y: Int) => x + y) `<$>` Nested(Id(Nothing[Int])) <*> Nested(Id(Just(6))),
      Nested(Id(Nothing[Int]))
    )

    val add: Int => Int => Int =
      x => y => x + y

    assertEquals(
      add `<$>` Nested(Id(Just(5))) <*> Nested(Id(Just(6))),
      Nested(Id(Just(11)))
    )

    assertEquals(
      add `<$>` Nested(Id(Just(5))) <*> Nested(Id(Nothing[Int])),
      Nested(Id(Nothing[Int]))
    )

    assertEquals(
      add `<$>` Nested(Id(Nothing[Int])) <*> Nested(Id(Just(6))),
      Nested(Id(Nothing[Int]))
    )
  }

  // TODO Id/Disjunction Applicative (as per the Maybe above)
}