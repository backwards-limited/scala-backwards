package com.backwards.text

import org.scalatest.{MustMatchers, WordSpec}

class StringOpsSpec extends WordSpec with MustMatchers with StringOps {
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