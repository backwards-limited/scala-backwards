package tech.backwards.categorytheory

import cats.data.Kleisli
import org.scalacheck.Gen
import org.scalactic.{Equality, TolerantNumerics}
import org.scalatest.matchers.must.Matchers
import org.scalatest.wordspec.AnyWordSpec
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks

class Chap4Spec extends AnyWordSpec with Matchers with ScalaCheckDrivenPropertyChecks {
  def safe(f: Double => Double): Double => Option[Double] =
    x => Option.when(x != 0)(f(x))

  def reciprocal(x: Double): Double =
    if (x == 0) throw new ArithmeticException("Attempted divide by zero") else 1.toDouble / x

  def sqrt(x: Double): Double =
    Math.sqrt(x)

  "Chap 4 reciprocal" should {
    "apply conforming to parameter invariants" in {
      forAll(Gen.posNum[Double]) { x: Double =>
        reciprocal(x) must ===(1 / x)
      }
    }

    "apply function causing an error because of 'divide by 0'" in {
      an [ArithmeticException] must be thrownBy reciprocal(0)
    }

    "apply a safe function that handles 'divide by 0'" in {
      safe(reciprocal)(0) must ===(None)
    }
  }

  "Chap 4 sqrt" should {
    "apply conforming to parameter invariants" in {
      forAll(Gen.posNum[Double]) { x: Double =>
        sqrt(x) must ===(Math.sqrt(x))
      }
    }
  }

  "Chap 4 composing reciprocal and sqrt with Kleisli" should {
    "apply safely with tolerance" in {
      val k: Kleisli[Option, Double, Double] =
        Kleisli(safe(reciprocal)) flatMapF safe(sqrt)

      val Some(result) = k.run(2)

      result must ===(0.707 +- 0.01)
    }

    "apply safely with implicit tolerance" in {
      implicit val doubleEquality: Equality[Double] =
        TolerantNumerics.tolerantDoubleEquality(0.01)

      val k: Kleisli[Option, Double, Double] =
        Kleisli(safe(reciprocal)) flatMapF safe(sqrt)

      val Some(result) = k.run(2)

      result must ===(0.707)
    }
  }
}