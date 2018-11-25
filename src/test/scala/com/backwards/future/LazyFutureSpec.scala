package com.backwards.future

import scala.concurrent.{Future, Promise}
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
  }
}