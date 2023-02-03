package tech.backwards.fp.learn

import munit.ScalaCheckSuite
import org.scalacheck.Prop._
import org.scalacheck.Test
import tech.backwards.fp.learn.Writer._

class WriterApplicativeSuite extends ScalaCheckSuite {
  override def scalaCheckTestParameters: Test.Parameters =
    super.scalaCheckTestParameters.withMinSuccessfulTests(100).withMaxDiscardRatio(10)

  property("Writer Applicative pure") {
    assertEquals(
      Applicative[Writer[String, *]].pure(5).run(),
      "" -> 5
    )

    assertEquals(
      Applicative[Writer[List[String], *]].pure(5).run(),
      Nil -> 5
    )
  }

  property("Writer Applicative pure syntax") {
    import tech.backwards.fp.learn.Applicative.syntax._

    assertEquals(
      5.pure[Writer[String, *]].run(),
      "" -> 5
    )

    assertEquals(
      5.pure[Writer[List[String], *]].run(),
      Nil -> 5
    )
  }

  property("Writer (String accumulator) Applicative ap") {
    import tech.backwards.fp.learn.Functor.syntax._

    val add: Int => Int => Int => Int =
      x => y => z => x + y + z

    val addFirstPartiallyApplied: Writer[String, Int => Int => Int] =
      Functor[Writer[String, *]].fmap(tell("a").as(5))(add)

    val addSecondPartiallyApplied: Writer[String, Int => Int] =
      Applicative[Writer[String, *]].ap(addFirstPartiallyApplied)(tell("b").as(10))

    val addThirdAndLastPartiallyApplied: Writer[String, Int] =
      Applicative[Writer[String, *]].ap(addSecondPartiallyApplied)(tell("c").as(20))

    assertEquals(
      addThirdAndLastPartiallyApplied.run(),
      "abc" -> 35
    )
  }

  property("Writer (List accumulator) Applicative ap") {
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
  }

  /*property("Disjunction Right Applicative ap syntax") {
    import tech.backwards.fp.learn.Applicative.syntax.function._
    import tech.backwards.fp.learn.Functor.syntax._

    val add: Int => Int => Int => Int =
      x => y => z => x + y + z

    assertEquals(
      Right(5).fmap(add).ap(Right(10)).ap(Right(20)),
      Right(35)
    )

    assertEquals(
      Right(5) fmap add ap Right(10) ap Right(20),
      Right(35)
    )

    assertEquals(
      Right(5) `<$>` add <*> Right(10) <*> Right(20),
      Right(35)
    )
  }*/

  /*property("Disjunction Left Applicative ap") {
    val add: Int => Int => Int => Int =
      x => y => z => x + y + z

    val addFirstPartiallyApplied: Disjunction[String, Int => Int => Int] =
      Functor[Disjunction[String, *]].fmap(Right(5))(add)

    val addSecondPartiallyApplied: Disjunction[String, Int => Int] =
      Applicative[Disjunction[String, *]].ap(addFirstPartiallyApplied)(Left("foo"))

    val addThirdAndLastPartiallyApplied: Disjunction[String, Int] =
      Applicative[Disjunction[String, *]].ap(addSecondPartiallyApplied)(Right(20))

    assertEquals(
      addThirdAndLastPartiallyApplied,
      Left("foo")
    )
  }*/

  /*property("Disjunction Left Applicative ap syntax") {
    import tech.backwards.fp.learn.Applicative.syntax.function._
    import tech.backwards.fp.learn.Disjunction.syntax._
    import tech.backwards.fp.learn.Functor.syntax._

    val add: Int => Int => Int => Int =
      x => y => z => x + y + z

    assertEquals(
      5.right[String].fmap(add).ap(Left("foo")).ap(Right(20)),
      Left("foo")
    )

    assertEquals(
      5.right[String] fmap add ap Right(10) ap Left("foo"),
      Left("foo")
    )

    assertEquals(
      5.right[String] `<$>` add <*> Left("foo") <*> Right(20),
      Left("foo")
    )
  }*/

  /*property("Disjunction Right Applicative ap function syntax") {
    import tech.backwards.fp.learn.Applicative.syntax.function._
    import tech.backwards.fp.learn.Functor.syntax.function._

    val add: Int => Int => Int => Int =
      x => y => z => x + y + z

    assertEquals(
      add `<$>` Right(5) <*> Right(10) <*> Right(20),
      Right(35)
    )

    assertEquals(
      ((x: Int) => (y: Int) => (z: Int) => x + y + z) `<$>` Right(5) <*> Right(10) <*> Right(20),
      Right(35)
    )

    assertEquals(
      ((x: Int, y: Int, z: Int) => x + y + z).curried `<$>` Right(5) <*> Right(10) <*> Right(20),
      Right(35)
    )
  }*/

  /*property("Disjunction Left Applicative ap function syntax") {
    import tech.backwards.fp.learn.Applicative.syntax.function._
    import tech.backwards.fp.learn.Disjunction.syntax._
    import tech.backwards.fp.learn.Functor.syntax.function._

    val add: Int => Int => Int => Int =
      x => y => z => x + y + z

    assertEquals(
      add `<$>` 5.right[String] <*> Left("foo") <*> Right(20),
      Left("foo")
    )

    assertEquals(
      ((x: Int) => (y: Int) => (z: Int) => x + y + z) `<$>` 5.right[String] <*> Right(10) <*> Left("foo"),
      Left("foo")
    )

    assertEquals(
      ((x: Int, y: Int, z: Int) => x + y + z).curried `<$>` 5.right[String] <*> Right(10) <*> Left("foo"),
      Left("foo")
    )
  }*/

  /*property("Disjunction Right Applicative ap function of arbitrary syntax") {
    import tech.backwards.fp.learn.Applicative.syntax.function._
    import tech.backwards.fp.learn.Functor.syntax.function._

    val add: Int => Int => Int => Int =
      x => y => z => x + y + z

    forAll((x: Int, y: Int, z: Int) =>
      assertEquals(
        add `<$>` Right(x) <*> Right(y) <*> Right(z),
        Right(x + y + z)
      )
    )
  }*/
}