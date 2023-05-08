package tech.backwards.fp.learn

import munit.ScalaCheckSuite
import org.scalacheck.Test

/**
 * First of the following IdT suites:
 *  - IdMonadTransformerSuite
 *  - IdMonadTransformerMaybeSuite
 *  - IdMonadTransformerDisjunctionSuite
 *  - IdMonadTransformerListSuite
 *  - IdMonadTransformerStateSuite
 *  - IdMonadTransformerWriterSuite
 */
class IdMonadTransformerSuite extends ScalaCheckSuite {
  override protected def scalaCheckTestParameters: Test.Parameters =
    super.scalaCheckTestParameters.withMinSuccessfulTests(100).withMaxDiscardRatio(10)

  property("IdT") {
    val transformer: IdT[Maybe, Int] =
      IdT(Just(Id(10)))

    assertEquals(
      transformer.value,
      Just(Id(10))
    )
  }

  property("IdT pure") {
    val transformer: IdT[Maybe, Int] =
      IdT.pure[Maybe, Int](10)

    assertEquals(
      transformer.value,
      Just(Id(10))
    )
  }
}