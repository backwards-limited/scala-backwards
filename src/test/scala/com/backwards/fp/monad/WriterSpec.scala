package com.backwards.fp.monad

import org.scalatest.{MustMatchers, WordSpec}
import com.backwards.fp.Writer

class WriterSpec extends WordSpec with MustMatchers {
  import com.backwards.fp.monad.MonadOps._
  import com.backwards.fp.monad.WriterOps._
  import com.backwards.fp.monoid.ListOps._
  //import com.backwards.fp.functor.FunctorOps._
  import com.backwards.fp.functor.WriterOps._

  "Writer Monad" should {
    "work" in {
      type MyWriter[A] = Writer[List[String], A]

      val writer = 1.pure[MyWriter].flatMap(x => 5.pure[MyWriter])
      println(writer.run())

      /*
      val writer = for {
        x <- 1.pure[WriterString]
        _ <- tell(List("een"))
        y <- 2.pure[WriterString]
        _ <- tell(List("twee"))
        z <- 3.pure[WriterString]
        _ <- tell(List("drie"))
      } yield (x, y, z)

      writer.run() mustBe (List("een", "twee", "drie"), (1, 2, 3))
      */
    }
  }
}