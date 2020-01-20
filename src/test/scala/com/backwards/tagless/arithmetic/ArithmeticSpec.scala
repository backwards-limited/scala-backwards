package com.backwards.tagless.arithmetic

import org.scalatest.matchers.must.Matchers
import org.scalatest.wordspec.AnyWordSpec

class ArithmeticSpec extends AnyWordSpec with Matchers {
  "Initial Alegbra" should {
    "evaluate" in {
      import com.backwards.tagless.arithmetic.Arithmetic.ADT._

      // 3 * 3 + 1 - 5
      val exp = Sub(Add(Mul(Lit(3), Lit(3)), Lit(1)), Lit(5))

      val value = evaluate(exp)
      value mustBe 5
    }
  }

  "Final Algebra" should {
    "evaluate" in {
      import com.backwards.tagless.arithmetic.Arithmetic.Tagless.Evaluate._

      val value = sub(add(mul(lit(3), lit(3)), lit(1)), lit(5))
      value mustBe 5
    }
  }

  "Equivalence" should {
    import com.backwards.tagless.arithmetic.Arithmetic.ADT._
    import com.backwards.tagless.arithmetic.Arithmetic._

    "ADT -> Tagless" in {
      val adtExp = Sub(Add(Mul(Lit(3), Lit(3)), Lit(1)), Lit(5))

      evaluate(adtExp) mustBe translate(adtExp, Tagless.Evaluate)
    }

    "Tagless -> ADT" in {
      import com.backwards.tagless.arithmetic.Arithmetic.Reify
      import com.backwards.tagless.arithmetic.Arithmetic.Tagless.Evaluate

      def evaluate[A](exp: Tagless.Exp[A]): A = {
        import exp._

        sub(add(mul(lit(3), lit(3)), lit(1)), lit(5))
      }

      evaluate(Evaluate) mustBe ADT.evaluate(evaluate(Reify))
    }
  }
}