package com.backwards.variance

import org.scalatest.matchers.must.Matchers
import org.scalatest.wordspec.AnyWordSpec

/**
  * Checking that a snippet of code does not compile
  * Often when creating libraries you may wish to ensure that certain arrangements of code that represent potential “user errors” do not compile, so that your library is more error resistant. ScalaTest Matchers trait includes the following syntax for that purpose:
  *
  * "val a: String = 1" mustNot compile
  * If you want to ensure that a snippet of code does not compile because of a type error (as opposed to a syntax error), use:
  *
  * "val a: String = 1" mustNot typeCheck
  * Note that the shouldNot typeCheck syntax will only succeed if the given snippet of code does not compile because of a type error. A syntax error will still result on a thrown TestFailedException.
  *
  * If you want to state that a snippet of code does compile, you can make that more obvious with:
  *
  * "val a: Int = 1" must compile
  * Although the previous three constructs are implemented with macros that determine at compile time whether the snippet of code represented by the string does or does not compile, errors are reported as test failures at runtime.
  */
class VarianceSpec extends AnyWordSpec with Matchers {
  class CSuper

  class C extends CSuper

  class CSub extends C
  class C1Sub extends C

  "" should {
    "" in {
      def func(f: C => C) = f(new C)

      val f: C1Sub => C =
        c => c

      "func(f)" mustNot compile
    }
  }
}