package com.backwards.fp.functor

import org.scalatest.MustMatchers
import org.scalatest.wordspec.AnyWordSpec
import com.backwards.fp.Writer

class WriterSpec extends AnyWordSpec with MustMatchers {
  import com.backwards.fp.functor.WriterOps._
  import com.backwards.fp.monoid.ListOps._

  "Writer Functor" should {
    "work" in {
      val writer = Writer(() => (List("een"), 10)) fmap { x: Int => x + 20 }

      writer.run() mustBe (List("een"), 30)
    }

    "obey identity" in {
      val writer = Writer(() => (List("een"), 10)) fmap identity

      writer.run() mustBe (List("een"), 10)
    }

    "obey composition" in {
      val f = { x: Int => x + 1 }
      val g = { x: Int => x * 2 }
      val a = Writer(() => (List("een"), 10)) fmap f fmap g

      (a fmap f fmap g).run() mustBe (a fmap (f andThen g)).run()
    }
  }

  "Writer Functor using <$>" should {
    "work" in {
      val writer = { x: Int => x + 20 } `<$>` Writer(() => (List("een"), 10))

      writer.run() mustBe (List("een"), 30)
    }
  }
}