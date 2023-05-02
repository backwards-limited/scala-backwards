package tech.backwards.fp.learn

import munit.ScalaCheckSuite
import org.scalacheck.Test

class MonadTransformerSuite extends ScalaCheckSuite {
  override protected def scalaCheckTestParameters: Test.Parameters =
    super.scalaCheckTestParameters.withMinSuccessfulTests(100).withMaxDiscardRatio(10)
}