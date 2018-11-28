package com.backwards.fp.monad

import org.scalatest.{MustMatchers, WordSpec}
import com.backwards.fp.State

class StateSpec extends WordSpec with MustMatchers {
  import com.backwards.fp.functor.StateOps._
  import com.backwards.fp.monad.MonadOps._

  type MyState[A] = State[String, A]

  "State Monad" should {
    "work" in {
      // TODO - Taking this import up to the others causes a "flatMap" not found error!!!
      import com.backwards.fp.monad.StateOps._

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