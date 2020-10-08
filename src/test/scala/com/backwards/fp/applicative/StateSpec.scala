package com.backwards.fp.applicative

import org.scalatest.matchers.must.Matchers
import org.scalatest.wordspec.AnyWordSpec
import com.backwards.fp.state.State

class StateSpec extends AnyWordSpec with Matchers {
  import com.backwards.fp.applicative.StateOps._
  import com.backwards.fp.functor.StateOps._

  def countAndRemove(char: Char): State[String, Int] = State { s: String =>
    val (chars, other) = s.partition(_ == char)
    (other, chars.length)
  }

  "State Applicative" should {
    "work" in {
      val state1: State[String, Int => Int => (Int, Int, Int)] =
        countAndRemove('a') fmap ((x: Int, y: Int, z: Int) => (x, y, z)).curried

      val state2: State[String, Int => (Int, Int, Int)] =
        state1 <*> 20.pure[String]

      val state3: State[String, (Int, Int, Int)] =
        state2 <*> countAndRemove('b')

      state3.run("abracadabra") mustBe ("rcdr", (5, 20, 2))
    }

    /* Note the extra paratheses just to keep IntelliJ happy */
    "work again" in {
      val state: State[String, (Int, Int, Int)] =
        (((x: Int) => (y: Int) => (z: Int) => (x, y, z)) `<$>` countAndRemove('a')) <*> 20.pure[String] <*> countAndRemove('b')

      state.run("abracadabra") mustBe ("rcdr", (5, 20, 2))
    }
  }
}