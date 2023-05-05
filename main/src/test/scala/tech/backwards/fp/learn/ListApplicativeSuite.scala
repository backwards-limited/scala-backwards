package tech.backwards.fp.learn

import munit.ScalaCheckSuite
import org.scalacheck.Prop._
import org.scalacheck.Test

class ListApplicativeSuite extends ScalaCheckSuite {
  override def scalaCheckTestParameters: Test.Parameters =
    super.scalaCheckTestParameters.withMinSuccessfulTests(100).withMaxDiscardRatio(10)

  property("List Applicative pure")(
    assertEquals(
      Applicative[List].pure(5),
      List(5)
    )
  )

  property("List Applicative pure syntax") {
    import tech.backwards.fp.learn.Applicative.syntax._

    assertEquals(
      5.pure[List],
      List(5)
    )
  }

  property("List Applicative ap") {
    val add: Int => Int => Int =
      x => y => x + y

    val addFirstPartiallyApplied: List[Int => Int] =
      Functor[List].fmap(List(1, 11))(add)

    val addSecondPartiallyApplied: List[Int] =
      Applicative[List].ap(addFirstPartiallyApplied)(List(3, 4, 5))

    assertEquals(
      addSecondPartiallyApplied,
      List(
        4, 5, 6,
        14, 15, 16
      )
    )
  }

  property("List Applicative ap syntax") {
    import tech.backwards.fp.learn.Applicative.syntax._
    import tech.backwards.fp.learn.Functor.syntax._

    val add: Int => Int => Int =
      x => y => x + y

    assertEquals(
      List(1, 11).fmap(add).ap(List(3, 4, 5)),
      List(
        4, 5, 6,
        14, 15, 16
      )
    )

    assertEquals(
      List(1, 11) fmap add ap List(3, 4, 5),
      List(
        4, 5, 6,
        14, 15, 16
      )
    )

    assertEquals(
      List(1, 11) `<$>` add <*> List(3, 4, 5),
      List(
        4, 5, 6,
        14, 15, 16
      )
    )
  }

  property("List Nil Applicative ap") {
    val add: Int => Int => Int =
      x => y => x + y

    val addFirstPartiallyApplied: List[Int => Int] =
      Functor[List].fmap(List(1, 11))(add)

    val addSecondPartiallyApplied: List[Int] =
      Applicative[List].ap(addFirstPartiallyApplied)(Nil)

    assertEquals(
      addSecondPartiallyApplied,
      Nil
    )
  }

  property("List Nil Applicative ap syntax") {
    import tech.backwards.fp.learn.Applicative.syntax._
    import tech.backwards.fp.learn.Functor.syntax._

    val add: Int => Int => Int =
      x => y => x + y

    assertEquals(
      List(1, 11).fmap(add).ap(Nil),
      Nil
    )

    assertEquals(
      List.empty[Int] fmap add ap List(3, 4, 5),
      Nil
    )

    assertEquals(
      List(1, 11) `<$>` add <*> Nil,
      Nil
    )
  }

  property("List Applicative ap function syntax") {
    import tech.backwards.fp.learn.Applicative.syntax._
    import tech.backwards.fp.learn.Functor.syntax.function._

    val add: Int => Int => Int =
      x => y => x + y

    assertEquals(
      add `<$>` List(1, 11) <*> List(3, 4, 5),
      List(
        4, 5, 6,
        14, 15, 16
      )
    )

    assertEquals(
      ((x: Int) => (y: Int) => x + y) `<$>` List(1, 11) <*> List(3, 4, 5),
      List(
        4, 5, 6,
        14, 15, 16
      )
    )

    assertEquals(
      ((x: Int, y: Int) => x + y).curried `<$>` List(1, 11) <*> List(3, 4, 5),
      List(
        4, 5, 6,
        14, 15, 16
      )
    )
  }

  property("List Nil Applicative ap function syntax") {
    import tech.backwards.fp.learn.Applicative.syntax._
    import tech.backwards.fp.learn.Functor.syntax.function._

    val add: Int => Int => Int =
      x => y => x + y

    assertEquals(
      add `<$>` List.empty[Int] <*> List(3, 4, 5),
      Nil
    )

    assertEquals(
      ((x: Int) => (y: Int) => x + y) `<$>` List(1, 11) <*> Nil,
      Nil
    )

    assertEquals(
      ((x: Int, y: Int) => x + y).curried `<$>` List(1, 11) <*> Nil,
      Nil
    )
  }

  property("List Applicative ap function of arbitrary syntax") {
    import tech.backwards.fp.learn.Applicative.syntax._
    import tech.backwards.fp.learn.Functor.syntax.function._

    val add: Int => Int => Int =
      x => y => x + y

    forAll((x: Int, y: Int) =>
      assertEquals(
        add `<$>` List(x, y) <*> List(x + 10, y + 10),
        List(
          x + x + 10, x + y + 10,
          y + x + 10, y + y + 10
        )
      )
    )
  }
}