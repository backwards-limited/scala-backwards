package com.backwards.fp.functor

import org.scalatest.MustMatchers
import org.scalatest.wordspec.AnyWordSpec
import com.backwards.fp.State

class StateSpec extends AnyWordSpec with MustMatchers {
  import com.backwards.fp.functor.StateOps._

  "State" should {
    "put" in {
      State.put(20).run(10) mustBe (20, ())
    }

    "get" in {
      State.get.run(10) mustBe (10, 10)

      State.get.eval(10) mustBe 10
    }

    "modify" in {
      State.modify((s: String) => s + " Baby").run("Ye") mustBe ("Ye Baby", ())
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