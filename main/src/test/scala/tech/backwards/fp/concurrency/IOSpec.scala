package tech.backwards.fp.concurrency

import cats.effect.IO
import cats.effect.unsafe.implicits.global
import cats.implicits._
import org.scalatest.matchers.must.Matchers
import org.scalatest.wordspec.AnyWordSpec

class IOSpec extends AnyWordSpec with Matchers {
  // Cats Effect 2 - Upgraded to Cats Effect 3
  /*val ec = ExecutionContext.fromExecutor(Executors.newCachedThreadPool())
  implicit val contextShift: ContextShift[IO] = IO.contextShift(ec)*/

  "IO" should {
    "run sequentially" in {
      val job1 = IO(1)
      val job2 = IO(2)

      val jobsSequential = job1.flatMap(one => job2.map(two => (one, two)))

      val fiber = jobsSequential.start.unsafeRunSync()
      fiber.joinWithNever.unsafeRunSync() mustBe (1, 2)

      val jobsSequentialAlternative = (job1, job2).tupled
      jobsSequentialAlternative.start.unsafeRunSync().joinWithNever.unsafeRunSync() mustBe (1, 2)
    }

    "run concurrently manually" in {
      val job1: IO[Int] = IO(1)
      val job2: IO[Int] = IO(2)

      val jobsConcurrent: IO[(Int, Int)] =
        for {
          fiber1 <- job1.start
          fiber2 <- job2.start
          one <- fiber1.joinWithNever
          two <- fiber2.joinWithNever
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