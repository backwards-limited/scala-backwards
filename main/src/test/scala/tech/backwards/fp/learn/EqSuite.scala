package tech.backwards.fp.learn

import munit.ScalaCheckSuite
import org.scalacheck.Test

class EqSuite extends ScalaCheckSuite {
  override def scalaCheckTestParameters: Test.Parameters =
    super.scalaCheckTestParameters.withMinSuccessfulTests(100).withMaxDiscardRatio(10)

  property("Eq of Ints") {
    assert(Eq.eq(1, 1))
    assert(!Eq.eq(1, 2))
  }

  property("Eq of Ints with syntax") {
    import tech.backwards.fp.learn.Eq.syntax._

    assert(1 ==== 1)
    assert(1 !=== 2)
  }

  property("Eq of Strings") {
    assert(Eq.eq("asd", "asd"))
    assert(!Eq.eq("dsa", "qwerty"))
  }

  property("Eq of Strings with syntax") {
    import tech.backwards.fp.learn.Eq.syntax._

    assert("asd" ==== "asd")
    assert("dsa"!=== "qwerty")
  }

  property("Eq of List of Int") {
    assert(Eq.eq(List(1, 2, 3), List(1, 2, 3)))
    assert(!Eq.eq(List(1, 2, 3), List(3, 4, 5)))
    assert(!Eq.eq(List(1, 2), List(1, 2, 3)))
    assert(!Eq.eq(List(1, 2, 3), List(1, 2)))
  }

  property("Eq of List of Int with syntax") {
    import tech.backwards.fp.learn.Eq.syntax._

    assert(List(1, 2, 3) ==== List(1, 2, 3))
    assert(List(1, 2, 3) !=== List(3, 4, 5))
    assert(List(1, 2) !=== List(1, 2, 3))
    assert(List(1, 2, 3) !=== List(1, 2))
  }

  property("Eq of List of String") {
    assert(Eq.eq(List("aa", "bbb", "ccc"), List("aa", "bbb", "ccc")))
    assert(!Eq.eq(List("aa", "bbb", "ccc"), List("a", "bb", "cc")))
  }

  property("Eq of List of String with syntax") {
    import tech.backwards.fp.learn.Eq.syntax._

    assert(List("aa", "bbb", "ccc") ==== List("aa", "bbb", "ccc"))
    assert(List("aa", "bbb", "ccc") !=== List("a", "bb", "cc"))
  }

  property("Eq of different types should not compile")(
    assertNoDiff(
      compileErrors("""Eq.eq("asd", 5)"""),
      """|error: could not find implicit value for evidence parameter of type tech.backwards.fp.learn.Eq[Any]
         |Eq.eq("asd", 5)
         |     ^
         |""".stripMargin.stripLineEnd
    )
  )
}