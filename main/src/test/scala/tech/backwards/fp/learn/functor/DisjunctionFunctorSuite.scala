package tech.backwards.fp.learn.functor

import munit.ScalaCheckSuite
import org.scalacheck.Prop._
import org.scalacheck.Test

class DisjunctionFunctorSuite extends ScalaCheckSuite {
  override def scalaCheckTestParameters: Test.Parameters =
    super.scalaCheckTestParameters.withMinSuccessfulTests(100).withMaxDiscardRatio(10)

  property("Left Disjunction Functor fmap")(
    assertEquals(
      Functor[Left[String, *]].fmap(Left[String, Int]("foo"))(_ + 1),
      Left[String, Int]("foo")
    )
  )

  property("Left Disjunction Functor fmap syntax") {
    import tech.backwards.fp.learn.functor.Functor.syntax._

    assertEquals(
      Left[String, Int]("foo").fmap(_ + 1),
      Left[String, Int]("foo")
    )

    assertEquals(
      Left[String, Int]("foo") `<$>` (_ + 1),
      Left[String, Int]("foo")
    )
  }

  property("Right Disjunction Functor fmap")(
    assertEquals(
      Functor[Right[String, *]].fmap(Right[String, Int](10))(_ + 1),
      Right[String, Int](11)
    )
  )

  property("Right Disjunction Functor fmap syntax") {
    import tech.backwards.fp.learn.functor.Functor.syntax._

    assertEquals(
      Right[String, Int](10).fmap(_ + 1),
      Right[String, Int](11)
    )

    assertEquals(
      Right[String, Int](10) `<$>` (_ + 1),
      Right[String, Int](11)
    )
  }

  property("Disjunction Functor fmap syntax") {
    import tech.backwards.fp.learn.functor.Functor.syntax._

    val leftDisjunction: Disjunction[String, Int] =
      Left[String, Int]("foo")

    assertEquals(
      leftDisjunction.fmap(_ + 1),
      leftDisjunction
    )

    assertEquals(
      leftDisjunction `<$>` (_ + 1),
      leftDisjunction
    )

    val rightDisjunction: Disjunction[String, Int] =
      Right[String, Int](10)

    assertEquals(
      rightDisjunction.fmap(_ + 1),
      Right[String, Int](11)
    )

    assertEquals(
      rightDisjunction `<$>` (_ + 1),
      Right[String, Int](11)
    )
  }

  property("Left Disjunction Functor fmap of function syntax") {
    import tech.backwards.fp.learn.functor.Functor.syntax.function._

    assertEquals(
      { x: Int => x + 1 } fmap Left[String, Int]("foo"),
      Left[String, Int]("foo")
    )

    assertEquals(
      { x: Int => x + 1 } `<$>` Left[String, Int]("foo"),
      Left[String, Int]("foo")
    )
  }

  property("Right Disjunction Functor fmap of function syntax") {
    import tech.backwards.fp.learn.functor.Functor.syntax.function._

    assertEquals(
      { x: Int => x + 1 } fmap Right[String, Int](10),
      Right[String, Int](11)
    )

    assertEquals(
      { x: Int => x + 1 } `<$>` Right[String, Int](10),
      Right[String, Int](11)
    )
  }

  property("Right Disjunction Functor fmap of arbitrary syntax") {
    import tech.backwards.fp.learn.functor.Functor.syntax._

    forAll((x: Int) =>
      assertEquals(
        Right[String, Int](x).fmap(_ + 1),
        Right[String, Int](x + 1)
      )
    )
  }

  property("Left Disjunction Functor fmap - obey identity") {
    import tech.backwards.fp.learn.functor.Functor.syntax._

    assertEquals(
      Left("foo") fmap identity,
      Left("foo")
    )
  }

  property("Right Disjunction Functor fmap - obey identity") {
    import tech.backwards.fp.learn.functor.Functor.syntax._

    assertEquals(
      Right[String, Int](10) fmap identity,
      Right[String, Int](10)
    )
  }

  property("Right Disjunction Functor fmap syntax - obey composition") {
    import tech.backwards.fp.learn.functor.Functor.syntax._

    val f: Int => Int =
      _ + 2

    val g: Int => Int =
      _ * 3

    assertEquals(
      Right[String, Int](10) `<$>` f `<$>` g,
      Right[String, Int](10).fmap(f andThen g)
    )
  }
}