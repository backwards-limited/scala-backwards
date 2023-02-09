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

  /*property("Writer (List accumulator) Applicative ap") {
    import tech.backwards.fp.learn.Functor.syntax._

    val add: Int => Int => Int => Int =
      x => y => z => x + y + z

    val addFirstPartiallyApplied: Writer[List[String], Int => Int => Int] =
      Functor[Writer[List[String], *]].fmap(tell(List("a")).as(5))(add)

    val addSecondPartiallyApplied: Writer[List[String], Int => Int] =
      Applicative[Writer[List[String], *]].ap(addFirstPartiallyApplied)(tell(List("b")).as(10))

    val addThirdAndLastPartiallyApplied: Writer[List[String], Int] =
      Applicative[Writer[List[String], *]].ap(addSecondPartiallyApplied)(tell(List("c")).as(20))

    assertEquals(
      addThirdAndLastPartiallyApplied.run(),
      List("a", "b", "c") -> 35
    )
  }*/

  /*property("Writer (String accumulator) Applicative ap syntax") {
    import tech.backwards.fp.learn.Applicative.syntax.function._
    import tech.backwards.fp.learn.Functor.syntax._

    val add: Int => Int => Int => Int =
      x => y => z => x + y + z

    assertEquals(
      tell("a").as(5).fmap(add).ap(tell("b").as(10)).ap(tell("c").as(20)).run(),
      "abc" -> 35
    )

    assertEquals(
      (tell("a").as(5) fmap add ap tell("b").as(10) ap tell("c").as(20)).run(),
      "abc" -> 35
    )

    assertEquals(
      (tell("a").as(5) `<$>` add <*> tell("b").as(10) <*> tell("c").as(20)).run(),
      "abc" -> 35
    )
  }*/

  /*property("Writer (List accumulator) Applicative ap syntax") {
    import tech.backwards.fp.learn.Applicative.syntax.function._
    import tech.backwards.fp.learn.Functor.syntax._

    val add: Int => Int => Int => Int =
      x => y => z => x + y + z

    assertEquals(
      tell(List("a")).as(5).fmap(add).ap(tell(List("b")).as(10)).ap(tell(List("c")).as(20)).run(),
      List("a", "b", "c") -> 35
    )

    assertEquals(
      (tell(List("a")).as(5) fmap add ap tell(List("b")).as(10) ap tell(List("c")).as(20)).run(),
      List("a", "b", "c") -> 35
    )

    assertEquals(
      (tell(List("a")).as(5) `<$>` add <*> tell(List("b")).as(10) <*> tell(List("c")).as(20)).run(),
      List("a", "b", "c") -> 35
    )
  }*/

  /*property("Writer (String accumulator) Applicative ap function syntax") {
    import tech.backwards.fp.learn.Applicative.syntax.function._
    import tech.backwards.fp.learn.Functor.syntax.function._

    val add: Int => Int => Int => Int =
      x => y => z => x + y + z

    assertEquals(
      (add `<$>` Writer("a" -> 5) <*> Writer("b" -> 10) <*> Writer("c" -> 20)).run(),
      "abc" -> 35
    )

    assertEquals(
      (((x: Int) => (y: Int) => (z: Int) => x + y + z) `<$>` Writer("a" -> 5) <*> Writer("b" -> 10) <*> Writer("c" -> 20)).run(),
      "abc" -> 35
    )

    assertEquals(
      (((x: Int, y: Int, z: Int) => x + y + z).curried `<$>` Writer("a" -> 5) <*> Writer("b" -> 10) <*> Writer("c" -> 20)).run(),
      "abc" -> 35
    )
  }*/

  /*property("Writer (List accumulator) Applicative ap function syntax") {
    import tech.backwards.fp.learn.Applicative.syntax.function._
    import tech.backwards.fp.learn.Functor.syntax.function._

    val add: Int => Int => Int => Int =
      x => y => z => x + y + z

    assertEquals(
      (add `<$>` Writer(List("a") -> 5) <*> Writer(List("b") -> 10) <*> Writer(List("c") -> 20)).run(),
      List("a", "b", "c") -> 35
    )

    assertEquals(
      (((x: Int) => (y: Int) => (z: Int) => x + y + z) `<$>` Writer(List("a") -> 5) <*> Writer(List("b") -> 10) <*> Writer(List("c") -> 20)).run(),
      List("a", "b", "c") -> 35
    )

    assertEquals(
      (((x: Int, y: Int, z: Int) => x + y + z).curried `<$>` Writer(List("a") -> 5) <*> Writer(List("b") -> 10) <*> Writer(List("c") -> 20)).run(),
      List("a", "b", "c") -> 35
    )
  }*/

  /*property("Writer (String accumulator) Applicative ap function of arbitrary syntax") {
    import tech.backwards.fp.learn.Applicative.syntax.function._
    import tech.backwards.fp.learn.Functor.syntax.function._

    val add: Int => Int => Int => Int =
      x => y => z => x + y + z

    forAll((x: Int, y: Int, z: Int) =>
      assertEquals(
        (add `<$>` Writer("a" -> x) <*> Writer("b" -> y) <*> Writer("c" -> z)).run(),
        "abc" -> (x + y + z)
      )
    )
  }*/

  /*property("Writer (List accumulator) Applicative ap function of arbitrary syntax") {
    import tech.backwards.fp.learn.Applicative.syntax.function._
    import tech.backwards.fp.learn.Functor.syntax.function._

    val add: Int => Int => Int => Int =
      x => y => z => x + y + z

    forAll((x: Int, y: Int, z: Int) =>
      assertEquals(
        (add `<$>` Writer(List("a") -> x) <*> Writer(List("b") -> y) <*> Writer(List("c") -> z)).run(),
        List("a", "b", "c") -> (x + y + z)
      )
    )
  }*/
}