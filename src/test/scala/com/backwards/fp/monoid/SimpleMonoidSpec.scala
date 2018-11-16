package com.backwards.fp.monoid

import org.scalatest.{MustMatchers, WordSpec}

class SimpleMonoidSpec extends WordSpec with MustMatchers {
  import com.backwards.fp.monoid.MonoidOps._
  import com.backwards.fp.monoid.SimpleMonoid._

  "Sum monoid" should {
    "sum numbers" in {
      Sum(10) |+| Sum(20) mustBe Sum(30)
    }

    "obey identity" in {
      Sum(10) |+| Monoid.mzero[Sum] mustBe Sum(10)
      Monoid.mzero[Sum] |+| Sum(10) mustBe Sum(10)
    }

    "obey associativity" in {
      Sum(10) |+| Sum (20) |+| Sum(30) mustBe Sum(10) |+| (Sum(20) |+| Sum(30))
    }
  }

  "Product monoid" should {
    "multiply numbers" in {
      Product(10) |+| Product(20) mustBe Product(200)
    }

    "obey identity" in {
      Product(10) |+| Monoid.mzero[Product] mustBe Product(10)
      Monoid.mzero[Product] |+| Product(10) mustBe Product(10)
    }

    "obey associativity" in {
      Product(10) |+| Product (20) |+| Product(30) mustBe Product(10) |+| (Product(20) |+| Product(30))
    }
  }
}