package tech.backwards.fp.learn

import munit.ScalaCheckSuite
import org.scalacheck.Prop._
import org.scalacheck.Test

class MaybeApplicativeSuite extends ScalaCheckSuite {
  override def scalaCheckTestParameters: Test.Parameters =
    super.scalaCheckTestParameters.withMinSuccessfulTests(100).withMaxDiscardRatio(10)

  property("Maybe Applicative pure")(
    assertEquals(
      Applicative[Maybe].pure(5),
      Just(5)
    )
  )

  property("Maybe Applicative pure syntax") {
    import tech.backwards.fp.learn.Applicative.syntax._

    assertEquals(
      5.pure[Maybe],
      Just(5)
    )
  }

  property("Maybe Just Applicative ap") {
    val add: Int => Int => Int => Int =
      x => y => z => x + y + z

    val addFirstPartiallyApplied: Maybe[Int => Int => Int] =
      Functor[Maybe].fmap(Just(5))(add)

    val addSecondPartiallyApplied: Maybe[Int => Int] =
      Applicative[Maybe].ap(addFirstPartiallyApplied)(Just(10))

    val addThirdAndLastPartiallyApplied: Maybe[Int] =
      Applicative[Maybe].ap(addSecondPartiallyApplied)(Just(20))

    assertEquals(
      addThirdAndLastPartiallyApplied,
      Just(35)
    )
  }

  property("Maybe Just Applicative ap syntax") {
    import tech.backwards.fp.learn.Applicative.syntax.function._
    import tech.backwards.fp.learn.Functor.syntax._

    val add: Int => Int => Int => Int =
      x => y => z => x + y + z

    assertEquals(
      Just(5).fmap(add).ap(Just(10)).ap(Just(20)),
      Just(35)
    )

    assertEquals(
      Just(5) fmap add ap Just(10) ap Just(20),
      Just(35)
    )

    assertEquals(
      Just(5) `<$>` add <*> Just(10) <*> Just(20),
      Just(35)
    )
  }

  property("Maybe Nothing Applicative ap") {
    val add: Int => Int => Int => Int =
      x => y => z => x + y + z

    val addFirstPartiallyApplied: Maybe[Int => Int => Int] =
      Functor[Maybe].fmap(Just(5))(add)

    val addSecondPartiallyApplied: Maybe[Int => Int] =
      Applicative[Maybe].ap(addFirstPartiallyApplied)(Nothing[Int])

    val addThirdAndLastPartiallyApplied: Maybe[Int] =
      Applicative[Maybe].ap(addSecondPartiallyApplied)(Just(20))

    assertEquals(
      addThirdAndLastPartiallyApplied,
      Nothing[Int]
    )
  }

  property("Maybe Nothing Applicative ap syntax") {
    import tech.backwards.fp.learn.Applicative.syntax.function._
    import tech.backwards.fp.learn.Functor.syntax._

    val add: Int => Int => Int => Int =
      x => y => z => x + y + z

    assertEquals(
      Just(5).fmap(add).ap(Nothing[Int]).ap(Just(20)),
      Nothing[Int]
    )

    assertEquals(
      Just(5) fmap add ap Just(10) ap Nothing[Int],
      Nothing[Int]
    )

    assertEquals(
      Just(5) `<$>` add <*> Nothing[Int] <*> Just(20),
      Nothing[Int]
    )
  }

  property("Maybe Just Applicative ap function syntax") {
    import tech.backwards.fp.learn.Applicative.syntax.function._
    import tech.backwards.fp.learn.Functor.syntax.function._

    val add: Int => Int => Int => Int =
      x => y => z => x + y + z

    assertEquals(
      add `<$>` Just(5) <*> Just(10) <*> Just(20),
      Just(35)
    )

    assertEquals(
      ((x: Int) => (y: Int) => (z: Int) => x + y + z) `<$>` Just(5) <*> Just(10) <*> Just(20),
      Just(35)
    )

    assertEquals(
      ((x: Int, y: Int, z: Int) => x + y + z).curried `<$>` Just(5) <*> Just(10) <*> Just(20),
      Just(35)
    )
  }

  property("Maybe Nothing Applicative ap function syntax") {
    import tech.backwards.fp.learn.Applicative.syntax.function._
    import tech.backwards.fp.learn.Functor.syntax.function._

    val add: Int => Int => Int => Int =
      x => y => z => x + y + z

    assertEquals(
      add `<$>` Just(5) <*> Nothing[Int] <*> Just(20),
      Nothing[Int]
    )

    assertEquals(
      ((x: Int) => (y: Int) => (z: Int) => x + y + z) `<$>` Just(5) <*> Just(10) <*> Nothing[Int],
      Nothing[Int]
    )

    assertEquals(
      ((x: Int, y: Int, z: Int) => x + y + z).curried `<$>` Just(5) <*> Just(10) <*> Nothing[Int],
      Nothing[Int]
    )
  }

  property("Maybe Just Applicative ap function of arbitrary syntax") {
    import tech.backwards.fp.learn.Applicative.syntax.function._
    import tech.backwards.fp.learn.Functor.syntax.function._

    val add: Int => Int => Int => Int =
      x => y => z => x + y + z

    forAll((x: Int, y: Int, z: Int) =>
      assertEquals(
        add `<$>` Just(x) <*> Just(y) <*> Just(z),
        Just(x + y + z)
      )
    )
  }
}