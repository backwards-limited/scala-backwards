package com.backwards.fp.monad

import org.scalatest.MustMatchers
import org.scalatest.wordspec.AnyWordSpec
import com.backwards.fp.Writer

class WriterSpec extends AnyWordSpec with MustMatchers {
  import com.backwards.fp.Writer._
  import com.backwards.fp.functor.WriterOps._
  import com.backwards.fp.monad.WriterOps._
  import com.backwards.fp.monoid.ListOps._

  "Writer Monad" should {
    "work" in {
      type MyWriter[A] = Writer[List[String], A]

      val writer = for {
        x <- 1.pure[MyWriter]
        _ <- tell(List("een"))
        y <- 2.pure[MyWriter]
        _ <- tell(List("twee"))
        z <- 3.pure[MyWriter]
        _ <- tell(List("drie"))
      } yield (x, y, z)

      writer.run() mustBe (List("een", "twee", "drie"), (1, 2, 3))
    }
  }
}