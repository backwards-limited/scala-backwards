package tech.backwards.fp.learn

import munit.ScalaCheckSuite
import tech.backwards.fp.learn.Writer._
import org.scalacheck.Prop._
import org.scalacheck.Test

class StateApplicativeSuite extends ScalaCheckSuite {
  override def scalaCheckTestParameters: Test.Parameters =
    super.scalaCheckTestParameters.withMinSuccessfulTests(100).withMaxDiscardRatio(10)

  property("State Applicative pure") {
    assertEquals(
      Applicative[State[String, *]].pure(5).run("hello"),
      "hello" -> 5
    )

    assertEquals(
      Applicative[State[List[String], *]].pure(5).run(List("hello")),
      List("hello") -> 5
    )
  }

  property("State Applicative pure syntax") {
    import tech.backwards.fp.learn.Applicative.syntax._

    assertEquals(
      5.pure[State[String, *]].run("hello"),
      "hello" -> 5
    )

    assertEquals(
      5.pure[State[List[String], *]].run(List("hello")),
      List("hello") -> 5
    )
  }

  property("State (String accumulator) Applicative ap") {
    val add: Int => Int => Int => Int =
      x => y => z => x + y + z

    val addFirstPartiallyApplied: State[String, Int => Int => Int] =
      Functor[State[String, *]].fmap(State(s => s"${s}a" -> 5))(add)

    val addSecondPartiallyApplied: State[String, Int => Int] =
      Applicative[State[String, *]].ap(addFirstPartiallyApplied)(State(s => s"${s}b" -> 10))

    val addThirdAndLastPartiallyApplied: State[String, Int] =
      Applicative[State[String, *]].ap(addSecondPartiallyApplied)(State(s => s"${s}c" -> 20))

    assertEquals(
      addThirdAndLastPartiallyApplied.run("hello"),
      "helloabc" -> 35
    )
  }

  property("State (List accumulator) Applicative ap") {
    val add: Int => Int => Int => Int =
      x => y => z => x + y + z

    val addFirstPartiallyApplied: State[List[String], Int => Int => Int] =
      Functor[State[List[String], *]].fmap(State(s => (s :+ "a") -> 5))(add)

    val addSecondPartiallyApplied: State[List[String], Int => Int] =
      Applicative[State[List[String], *]].ap(addFirstPartiallyApplied)(State(s => (s :+ "b") -> 10))

    val addThirdAndLastPartiallyApplied: State[List[String], Int] =
      Applicative[State[List[String], *]].ap(addSecondPartiallyApplied)(State(s => (s :+ "c") -> 20))

    assertEquals(
      addThirdAndLastPartiallyApplied.run(List("hello")),
      List("hello", "a", "b", "c") -> 35
    )
  }

  property("State (String accumulator) Applicative ap syntax") {
    import tech.backwards.fp.learn.Applicative.syntax._
    import tech.backwards.fp.learn.Functor.syntax._

    val add: Int => Int => Int => Int =
      x => y => z => x + y + z

    // Example of a longer approach to the following tests:
    val example: (String, Int) =
      State.modify[String](_ + "a").map(_ => 5).fmap(add)
        .ap(State.modify[String](_ + "b").map(_ => 10))
        .ap(State.modify[String](_ +"c").map(_ => 20)).run("hello")

    println(example)

    assertEquals(
      State((s: String) => s"${s}a" -> 5).fmap(add).ap(State((s: String) => s"${s}b" -> 10)).ap(State((s: String) => s"${s}c" -> 20)).run("hello"),
      "helloabc" -> 35
    )

    assertEquals(
      (State((s: String) => s"${s}a" -> 5) fmap add ap State((s: String) => s"${s}b" -> 10) ap State((s: String) => s"${s}c" -> 20)).run("hello"),
      "helloabc" -> 35
    )

    assertEquals(
      (State((s: String) => s"${s}a" -> 5) `<$>` add <*> State((s: String) => s"${s}b" -> 10) <*> State((s: String) => s"${s}c" -> 20)).run("hello"),
      "helloabc" -> 35
    )
  }

  property("State (List accumulator) Applicative ap syntax") {
    import tech.backwards.fp.learn.Applicative.syntax._
    import tech.backwards.fp.learn.Functor.syntax._

    val add: Int => Int => Int => Int =
      x => y => z => x + y + z

    // Example of a longer approach to the following tests:
    val example: (List[String], Int) =
      State.modify[List[String]](_ :+ "a").map(_ => 5).fmap(add)
        .ap(State.modify[List[String]](_ :+ "b").map(_ => 10))
        .ap(State.modify[List[String]](_ :+ "c").map(_ => 20)).run(List("hello"))

    println(example)

    assertEquals(
      State((s: List[String]) => (s :+ "a") -> 5).fmap(add).ap(State((s: List[String]) => (s :+ "b") -> 10)).ap(State((s: List[String]) => (s :+ "c") -> 20)).run(List("hello")),
      List("hello", "a", "b", "c") -> 35
    )

    assertEquals(
      (State((s: List[String]) => (s :+ "a") -> 5) fmap add ap State((s: List[String]) => (s :+ "b") -> 10) ap State((s: List[String]) => (s :+ "c") -> 20)).run(List("hello")),
      List("hello", "a", "b", "c") -> 35
    )

    assertEquals(
      (State((s: List[String]) => (s :+ "a") -> 5) `<$>` add <*> State((s: List[String]) => (s :+ "b") -> 10) <*> State((s: List[String]) => (s :+ "c") -> 20)).run(List("hello")),
      List("hello", "a", "b", "c") -> 35
    )
  }

  property("State (String accumulator) Applicative ap function syntax") {
    import tech.backwards.fp.learn.Applicative.syntax._
    import tech.backwards.fp.learn.Functor.syntax.function._

    val add: Int => Int => Int => Int =
      x => y => z => x + y + z

    assertEquals(
      (add `<$>` State((s: String) => s"${s}a" -> 5) <*> State((s: String) => s"${s}b" -> 10) <*> State((s: String) => s"${s}c" -> 20)).run("hello"),
      "helloabc" -> 35
    )

    assertEquals(
      (((x: Int) => (y: Int) => (z: Int) => x + y + z) `<$>` State((s: String) => s"${s}a" -> 5) <*> State((s: String) => s"${s}b" -> 10) <*> State((s: String) => s"${s}c" -> 20)).run("hello"),
      "helloabc" -> 35
    )

    assertEquals(
      (((x: Int, y: Int, z: Int) => x + y + z).curried `<$>` State((s: String) => s"${s}a" -> 5) <*> State((s: String) => s"${s}b" -> 10) <*> State((s: String) => s"${s}c" -> 20)).run("hello"),
      "helloabc" -> 35
    )
  }

  property("State (List accumulator) Applicative ap function syntax") {
    import tech.backwards.fp.learn.Applicative.syntax._
    import tech.backwards.fp.learn.Functor.syntax.function._

    val add: Int => Int => Int => Int =
      x => y => z => x + y + z

    assertEquals(
      (add `<$>` State((s: List[String]) => (s :+ "a") -> 5) <*> State((s: List[String]) => (s :+ "b") -> 10) <*> State((s: List[String]) => (s :+ "c") -> 20)).run(List("hello")),
      List("hello", "a", "b", "c") -> 35
    )

    assertEquals(
      (((x: Int) => (y: Int) => (z: Int) => x + y + z) `<$>` State((s: List[String]) => (s :+ "a") -> 5) <*> State((s: List[String]) => (s :+ "b") -> 10) <*> State((s: List[String]) => (s :+ "c") -> 20)).run(List("hello")),
      List("hello", "a", "b", "c") -> 35
    )

    assertEquals(
      (((x: Int, y: Int, z: Int) => x + y + z).curried `<$>` State((s: List[String]) => (s :+ "a") -> 5) <*> State((s: List[String]) => (s :+ "b") -> 10) <*> State((s: List[String]) => (s :+ "c") -> 20)).run(List("hello")),
      List("hello", "a", "b", "c") -> 35
    )
  }

  property("State (String accumulator) Applicative ap function of arbitrary syntax") {
    import tech.backwards.fp.learn.Applicative.syntax._
    import tech.backwards.fp.learn.Functor.syntax.function._

    val add: Int => Int => Int => Int =
      x => y => z => x + y + z

    forAll((x: Int, y: Int, z: Int) =>
      assertEquals(
        (add `<$>` State((s: String) => s"${s}a" -> x) <*> State((s: String) => s"${s}b" -> y) <*> State((s: String) => s"${s}c" -> z)).run("hello"),
        "helloabc" -> (x + y + z)
      )
    )
  }

  property("State (List accumulator) Applicative ap function of arbitrary syntax") {
    import tech.backwards.fp.learn.Applicative.syntax._
    import tech.backwards.fp.learn.Functor.syntax.function._

    val add: Int => Int => Int => Int =
      x => y => z => x + y + z

    forAll((x: Int, y: Int, z: Int) =>
      assertEquals(
        (add `<$>` Writer(List("a") -> x) <*> Writer(List("b") -> y) <*> Writer(List("c") -> z)).run(),
        List("a", "b", "c") -> (x + y + z)
      )
    )

    forAll((x: Int, y: Int, z: Int) =>
      assertEquals(
        (add `<$>` State((s: List[String]) => (s :+ "a") -> x) <*> State((s: List[String]) => (s :+ "b") -> y) <*> State((s: List[String]) => (s :+ "c") -> z)).run(List("hello")),
        List("hello", "a", "b", "c") -> (x + y + z)
      )
    )
  }
}