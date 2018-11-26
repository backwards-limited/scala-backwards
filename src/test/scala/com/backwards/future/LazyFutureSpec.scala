package com.backwards.future

import scala.concurrent.{Future, Promise}
import cats.Monad
import org.scalatest.{AsyncWordSpec, MustMatchers, OneInstancePerTest}

class LazyFutureSpec extends AsyncWordSpec with OneInstancePerTest with MustMatchers {
  val done = "done"

  val run: Promise[Boolean] = Promise[Boolean]()

  val lazyFuture: LazyFuture[String] = { _ =>
    run success true
    Future successful done
  }

  "Lazy Future" should {
    "wait to be run" in {
      run.isCompleted mustBe false
    }

    "run" in {
      lazyFuture.run().map { result =>
        result mustBe done
        run.isCompleted mustBe true
      }
    }

    "lift into itself" in {
      val lifted: LazyFuture[Int] = Monad[LazyFuture].point(42)

      lifted.run().map(_ mustBe 42)
    }

    "flatMap" in {
      val f: String => LazyFuture[String] =
        s => _ => Future.successful(s * 2)

      val flatMappedLazyFuture: LazyFuture[String] = Monad[LazyFuture].flatMap[String, String](lazyFuture)(f)

      flatMappedLazyFuture.run().map { result =>
        result mustBe done * 2
        run.isCompleted mustBe true
      }
    }
  }
}