package com.backwards.fp.functor

import org.scalatest.{MustMatchers, WordSpec}

class WriterSpec extends WordSpec with MustMatchers {
  import com.backwards.fp.functor.Writer._

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