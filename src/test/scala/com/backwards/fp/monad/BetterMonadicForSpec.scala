package com.backwards.fp.monad

import org.scalatest.matchers.must.Matchers
import org.scalatest.wordspec.AnyWordSpec

class BetterMonadicForSpec extends AnyWordSpec with Matchers {
  "" should {
    "" in {
      import cats.implicits._
      import cats.effect.IO

      def getCounts: IO[(Int, Int)] = IO(1 -> 2)

      val result: IO[Int] = for {
        (x, y) <- getCounts
      } yield x + y

      result.unsafeRunSync mustBe 3
    }
  }
}