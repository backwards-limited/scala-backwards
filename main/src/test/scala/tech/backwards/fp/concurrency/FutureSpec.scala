package tech.backwards.fp.concurrency

import scala.concurrent.Future
import org.scalatest.matchers.must.Matchers
import org.scalatest.wordspec.AsyncWordSpec

class FutureSpec extends AsyncWordSpec with Matchers {
  "Futures" should {
    "run sequentially because of def" in {
      def job1: Future[Int] = Future(1)
      def job2: Future[Int] = Future(2)

      val result = job1.flatMap(one => job2.map(two => one + two))
      result.map(_ mustBe 3)
    }

    "run concurrently because of val" in {
      val job1 = Future(1)
      val job2 = Future(2)

      val result = job1.flatMap(one => job2.map(two => one + two))
      result.map(_ mustBe 3)
    }
  }
}