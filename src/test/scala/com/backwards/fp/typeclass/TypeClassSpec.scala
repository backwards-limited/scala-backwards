package com.backwards.fp.typeclass

import org.scalatest.matchers.must.Matchers
import org.scalatest.wordspec.AnyWordSpec

class TypeClassSpec extends AnyWordSpec with Matchers {
  "Typeclass" should {
    "apply the int instance" in {
      import com.backwards.fp.typeclass.TypeClass._
      import com.backwards.fp.typeclass.TypeClassInstances._

      foo(5) mustEqual "int"
    }

    "apply the string instance" in {
      import com.backwards.fp.typeclass.TypeClass._
      import com.backwards.fp.typeclass.TypeClassInstances._

      foo("5") mustEqual "string"
    }
  }
}