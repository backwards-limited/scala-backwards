package com.backwards.fp.concurrency

import java.util.concurrent.{Executors, TimeUnit}
import scala.concurrent.{ExecutionContext, Future}
import cats.effect.{ContextShift, IO}
import org.scalatest.matchers.must.Matchers
import org.scalatest.wordspec.{AnyWordSpec, AsyncWordSpec}
import cats.implicits._

class IOSpec extends AnyWordSpec with Matchers {
  val ec = ExecutionContext.fromExecutor(Executors.newCachedThreadPool())
  implicit val contextShift: ContextShift[IO] = IO.contextShift(ec)

  "IO" should {
    "run sequentially" in {
      val job1 = IO(1)
      val job2 = IO(2)

      val jobsSequential = job1.flatMap(one => job2.map(two => (one, two)))

      val fiber = jobsSequential.start.unsafeRunSync()
      fiber.join.unsafeRunSync() mustBe (1, 2)

      val jobsSequentialAlternative = (job1, job2).tupled
      jobsSequentialAlternative.start.unsafeRunSync().join.unsafeRunSync() mustBe (1, 2)
    }

    "run concurrently manually" in {
      val job1 = IO(1)
      val job2 = IO(2)

      val jobsConcurrent: IO[(Int, Int)] = for {
        fiber1 <- job1.start
        fiber2 <- job2.start
        one <- fiber1.join
        two <- fiber2.join
      } yield (one, two)

      jobsConcurrent.unsafeRunSync() mustBe (1, 2)
    }

    "run concurrently higher level" in {
      val job1 = IO(1)
      val job2 = IO(2)

      val jobsConcurrent: IO[(Int, Int)] = (job1, job2).parTupled

      jobsConcurrent.unsafeRunSync() mustBe (1, 2)
    }
  }
}