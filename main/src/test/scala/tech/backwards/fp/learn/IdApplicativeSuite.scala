package tech.backwards.fp.learn

import munit.ScalaCheckSuite
import org.scalacheck.Prop._
import org.scalacheck.Test

class IdApplicativeSuite extends ScalaCheckSuite {
  override def scalaCheckTestParameters: Test.Parameters =
    super.scalaCheckTestParameters.withMinSuccessfulTests(100).withMaxDiscardRatio(10)

  property("Id Applicative pure")(
    assertEquals(
      Applicative[Id].pure(5),
      Id(5)
    )
  )

  property("Id Applicative pure syntax") {
    import tech.backwards.fp.learn.Applicative.syntax._

    assertEquals(
      5.pure[Id],
      Id(5)
    )
  }

  property("Id Applicative ap") {
    val add: Int => Int => Int => Int =
      x => y => z => x + y + z

    val addFirstPartiallyApplied: Id[Int => Int => Int] =
      Functor[Id].fmap(Id(5))(add)

    val addSecondPartiallyApplied: Id[Int => Int] =
      Applicative[Id].ap(addFirstPartiallyApplied)(Id(10))

    val addThirdAndLastPartiallyApplied: Id[Int] =
      Applicative[Id].ap(addSecondPartiallyApplied)(Id(20))

    assertEquals(
      addThirdAndLastPartiallyApplied,
      Id(35)
    )
  }

  property("Id Applicative ap syntax") {
    import tech.backwards.fp.learn.Applicative.syntax.function._
    import tech.backwards.fp.learn.Functor.syntax._

    val add: Int => Int => Int => Int =
      x => y => z => x + y + z

    assertEquals(
      Id(5).fmap(add).ap(Id(10)).ap(Id(20)),
      Id(35)
    )

    assertEquals(
      Id(5) fmap add ap Id(10) ap Id(20),
      Id(35)
    )

    assertEquals(
      Id(5) `<$>` add <*> Id(10) <*> Id(20),
      Id(35)
    )
  }

  property("Id Applicative ap function syntax") {
    import tech.backwards.fp.learn.Applicative.syntax.function._
    import tech.backwards.fp.learn.Functor.syntax.function._

    val add: Int => Int => Int => Int =
      x => y => z => x + y + z

    assertEquals(
      add `<$>` Id(5) <*> Id(10) <*> Id(20),
      Id(35)
    )

    assertEquals(
      ((x: Int) => (y: Int) => (z: Int) => x + y + z) `<$>` Id(5) <*> Id(10) <*> Id(20),
      Id(35)
    )

    assertEquals(
      ((x: Int, y: Int, z: Int) => x + y + z).curried `<$>` Id(5) <*> Id(10) <*> Id(20),
      Id(35)
    )
  }

  property("Id Applicative ap function of arbitrary syntax") {
    import tech.backwards.fp.learn.Applicative.syntax.function._
    import tech.backwards.fp.learn.Functor.syntax.function._

    val add: Int => Int => Int => Int =
      x => y => z => x + y + z

    forAll((x: Int, y: Int, z: Int) =>
      assertEquals(
        add `<$>` Id(x) <*> Id(y) <*> Id(z),
        Id(x + y + z)
      )
    )
  }
}