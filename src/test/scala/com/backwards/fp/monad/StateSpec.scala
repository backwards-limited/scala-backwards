package com.backwards.fp.monad

import org.scalatest.matchers.must.Matchers
import org.scalatest.wordspec.AnyWordSpec
import com.backwards.fp.state.Backwards.State

class StateSpec extends AnyWordSpec with Matchers {
  import com.backwards.fp.functor.StateOps._
  import com.backwards.fp.monad.StateOps._

  type MyState[A] = State[String, A]

  "State Monad" should {
    "work" in {
      val quickCheck = 10.pure[MyState].flatMap(_ => State.get[String]).flatMap(s => State.put(s + " Baby"))
      quickCheck.run("Ye") mustBe ("Ye Baby", ())

      val state = for {
        x <- 10.pure[MyState]
        os <- State.get[String]
        y <- 20.pure[MyState]
        _ <- State.put(os + " boom")
        z <- 30.pure[MyState]
        _ <- State.get[String] >>= { ns => State.put(ns + " baam") }
      } yield x + y + z

      state.run("boom") mustBe ("boom boom baam", 10 + 20 + 30)
    }
  }
}