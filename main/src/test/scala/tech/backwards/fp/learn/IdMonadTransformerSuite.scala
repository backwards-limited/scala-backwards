package tech.backwards.fp.learn

import munit.ScalaCheckSuite
import org.scalacheck.Test

class IdMonadTransformerSuite extends ScalaCheckSuite {
  override protected def scalaCheckTestParameters: Test.Parameters =
    super.scalaCheckTestParameters.withMinSuccessfulTests(100).withMaxDiscardRatio(10)

  property("Blah") {

    val v: IdT[Maybe, Int] =
      IdT(Just(Id(10)))

    println(v.value) // Maybe[Id[Int]]
    ???
  }

  property("Blah Blah") {

    val v: IdT[Maybe, Int] =
      IdT.pure[Maybe, Int](10)

    println(v.value) // Maybe[Id[Int]]
    ???
  }

  property("Blah Blah Blah") {

    import tech.backwards.fp.learn.Applicative.syntax._

    val v: IdT[Maybe, Int] =
      10.pure[IdT[Maybe, *]]

    println(v.value) // Maybe[Id[Int]]
    ???
  }
}