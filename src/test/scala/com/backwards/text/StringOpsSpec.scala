package com.backwards.text

import org.scalatest.matchers.must.Matchers
import org.scalatest.wordspec.AnyWordSpec

class StringOpsSpec extends AnyWordSpec with Matchers with StringOps {
  "Lower kebab" should {
    object ConsumerDemoChoosePartition1

    object ConsumerDemoChoosePartition2

    "extract from given string" in {
      lowerKebab(ConsumerDemoChoosePartition1.getClass.getSimpleName.takeWhile(_ != '$')) mustEqual "consumer-demo-choose-partition-1"
    }

    "extract from given class" in {
      lowerKebab(ConsumerDemoChoosePartition1.getClass) mustEqual "consumer-demo-choose-partition-1"

      lowerKebab(ConsumerDemoChoosePartition2.getClass) mustEqual "consumer-demo-choose-partition-2"
    }
  }
}