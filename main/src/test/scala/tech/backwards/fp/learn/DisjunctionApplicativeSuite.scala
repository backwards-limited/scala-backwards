package tech.backwards.fp.learn

import munit.ScalaCheckSuite
import org.scalacheck.Prop._
import org.scalacheck.Test

class DisjunctionApplicativeSuite extends ScalaCheckSuite {
  override def scalaCheckTestParameters: Test.Parameters =
    super.scalaCheckTestParameters.withMinSuccessfulTests(100).withMaxDiscardRatio(10)

  property("Disjunction Applicative pure")(
    assertEquals(
      Applicative[Disjunction[String, *]].pure(5),
      Right(5)
    )
  )

  property("Disjunction Applicative pure syntax") {
    import tech.backwards.fp.learn.Applicative.syntax._

    assertEquals(
      5.pure[Disjunction[String, *]],
      Right(5)
    )
  }

  property("Disjunction Right Applicative ap") {
    val add: Int => Int => Int => Int =
      x => y => z => x + y + z

    val addFirstPartiallyApplied: Disjunction[String, Int => Int => Int] =
      Functor[Disjunction[String, *]].fmap(Right(5))(add)

    val addSecondPartiallyApplied: Disjunction[String, Int => Int] =
      Applicative[Disjunction[String, *]].ap(addFirstPartiallyApplied)(Right(10))

    val addThirdAndLastPartiallyApplied: Disjunction[String, Int] =
      Applicative[Disjunction[String, *]].ap(addSecondPartiallyApplied)(Right(20))

    assertEquals(
      addThirdAndLastPartiallyApplied,
      Right(35)
    )
  }

  property("Disjunction Right Applicative ap syntax") {
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
  }

  property("Disjunction Left Applicative ap") {
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
  }

  property("Disjunction Left Applicative ap syntax") {
    import tech.backwards.fp.learn.Applicative.syntax.function._
    import tech.backwards.fp.learn.Functor.syntax._

    val add: Int => Int => Int => Int =
      x => y => z => x + y + z

    assertEquals(
      Right[String, Int](5).fmap(add).ap(Left("foo")).ap(Right(20)),
      Left("foo")
    )

    assertEquals(
      Right[String, Int](5) fmap add ap Right(10) ap Left("foo"),
      Left("foo")
    )

    assertEquals(
      Right[String, Int](5) `<$>` add <*> Left("foo") <*> Right(20),
      Left("foo")
    )
  }

  property("Disjunction Right Applicative ap function syntax") {
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
  }

  property("Disjunction Left Applicative ap function syntax") {
    import tech.backwards.fp.learn.Applicative.syntax.function._
    import tech.backwards.fp.learn.Functor.syntax.function._

    val add: Int => Int => Int => Int =
      x => y => z => x + y + z

    assertEquals(
      add `<$>` Right[String, Int](5) <*> Left("foo") <*> Right(20),
      Left("foo")
    )

    assertEquals(
      ((x: Int) => (y: Int) => (z: Int) => x + y + z) `<$>` Right[String, Int](5) <*> Right(10) <*> Left("foo"),
      Left("foo")
    )

    assertEquals(
      ((x: Int, y: Int, z: Int) => x + y + z).curried `<$>` Right[String, Int](5) <*> Right(10) <*> Left("foo"),
      Left("foo")
    )
  }

  property("Disjunction Right Applicative ap function of arbitrary syntax") {
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
  }
}