package com.backwards.variance

import scala.util.matching.Regex
import org.scalatest.matchers.must.Matchers
import org.scalatest.wordspec.AnyWordSpec

/**
 *
 */
class Variance2Spec extends AnyWordSpec with Matchers {
  class CSuper {
    def csuper = ???
  }

  class C extends CSuper {
    def c = ???
  }

  class CSub extends C {
    def csub = ???
  }

  var f: C => C = _

  "" should {
    "" in {
      f = (c: C) => new C
      f = (c: CSuper) => new CSub
      f = (c: CSuper) => new C
      f = (c: C) => new CSub

      // Following is a compilation error:
      "f = (c: CSub) => new CSuper" mustNot compile

      /*
      Let’s consider what would happen if we allowed that substitution for f.

      In this case, the actual f would only know how to handle CSub instances.
      However, the caller would believe that any C instance can be passed to f, so a runtime failure is likely when f is "surprised" i.e., it tries to call some method that is only defined for CSub, not C.

      CSuper
        ^
        C
        ^
       CSub

      f(new C) .... but someone made the above assignment e.g.

      val dodgyF = (csub: CSub) => csub.csub; new CSuper
                                          ^------------ Called on given C which does not have the method csub
      */

      /*
      Similarly, if the actual f can return a CSuper, it will "surprise" the caller with an instance that is outside the range of expected return values, the allowed instances of C.

      val c = f(new C) expects to get back a C
      c.c
        ^------- but a CSuper was actually returned by dodgyF which does not have the method c
      */

      /*
      This is why function arguments must be contravariant and return values must be covariant.

      Variance annotations only make sense on the type parameters for types, not for parameterized methods, because the annotations affect the behavior of subtyping. Methods aren't subtyped.

      For example, the simplified signature for the List.map method looks like this:
      sealed abstract class List[+A] ... { // mixin traits omitted ...
        def map[B](f: A => B): List[B] = {...}
        ...
      }

      There is no variance annotation on B and if you tried to add one, the compiler would throw an error.


      NOTE - only invariance is allowed for mutable types.
      */
    }
  }
}