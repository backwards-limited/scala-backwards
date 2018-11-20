package com.backwards.fp.functor

import org.scalatest.{MustMatchers, WordSpec}

class StateSpec extends WordSpec with MustMatchers {
  import com.backwards.fp.functor.State._

  "State" should {
    "put" in {
      State.put(20).run(10) mustBe (20, ())
    }

    "get" in {
      State.get.run(10) mustBe (10, 10)

      State.get.eval(10) mustBe 10
    }
  }

  "State Functor" should {
    "fmap" in {
      (State[String, Int](s => (s"extended $s", 10)) fmap { x: Int => x + 20 }).run("state") mustBe ("extended state", 30)
    }

    "obey identity" in {
      (State[String, Int](s => (s, 10)) fmap identity).run("state") mustBe ("state", 10)
    }

    "obey composition" in {
      val f = { x: Int => x + 20 }
      val g = { x: Int => x * 2 }

      (State[String, Int](s => (s, 10)) fmap f fmap g).run("state") mustBe (State[String, Int](s => (s, 10)) fmap { f andThen g }).run("state")
    }
  }

  "State Functor using <$>" should {
    "work" in {
      val state = { x: Int => x + 20 } `<$>` State[String, Int](s => (s"extended $s", 10))

      state.run("state") mustBe ("extended state", 30)
    }
  }
}