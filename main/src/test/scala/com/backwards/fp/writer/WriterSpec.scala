package com.backwards.fp.writer

import org.scalatest.matchers.must.Matchers
import org.scalatest.wordspec.AnyWordSpec

class WriterSpec extends AnyWordSpec with Matchers {
  import com.backwards.fp.writer.Writer._
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