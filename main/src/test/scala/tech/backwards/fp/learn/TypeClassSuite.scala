package tech.backwards.fp.learn

import munit.ScalaCheckSuite
import org.scalacheck.Prop._
import org.scalacheck.Test

class TypeClassSuite extends ScalaCheckSuite {
  override def scalaCheckTestParameters: Test.Parameters =
    super.scalaCheckTestParameters.withMinSuccessfulTests(100).withMaxDiscardRatio(10)

  property("Int instance of TypeClass")(
    forAll((i: Int) =>
      assertEquals(TypeClass.foo(i), s"Int: $i")
    )
  )

  property("Int instance of TypeClass with syntax") {
    import tech.backwards.fp.learn.TypeClass.syntax._

    forAll((i: Int) =>
      assertEquals(i.foo, s"Int: $i")
    )
  }

  property("String instance of TypeClass")(
    forAll((s: String) =>
      assertEquals(TypeClass.foo(s), s"String: $s")
    )
  )

  property("String instance of TypeClass with syntax") {
    import tech.backwards.fp.learn.TypeClass.syntax._

    forAll((s: String) =>
      assertEquals(s.foo, s"String: $s")
    )
  }
}