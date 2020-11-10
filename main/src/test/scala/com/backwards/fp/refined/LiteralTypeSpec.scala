package com.backwards.fp.refined

import eu.timepit.refined.api.Refined
import eu.timepit.refined.auto._
import eu.timepit.refined.boolean.And
import eu.timepit.refined.collection.{MaxSize, NonEmpty}
import eu.timepit.refined.numeric.Positive
import org.scalatest.matchers.must.Matchers
import org.scalatest.wordspec.AnyWordSpec

class LiteralTypeSpec extends AnyWordSpec with Matchers {
  "" should {
    "" in {
      // Before Scala 2.13 we would need the W notation (short for "witness"):
      // import refined.W
      // type Name = NonEmpty And MaxSize[W.`10`.T]

      type Name = NonEmpty And MaxSize[10]

      final case class Person(
        name: String Refined Name,
        age: Int Refined Positive
      )

      val person = Person("Bob", 99)

      // The following does not compile
      // val personWhoops = Person("Bob with a name that is too long", 99)
    }
  }
}