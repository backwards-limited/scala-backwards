package com.backwards.fp.state

import org.scalatest.matchers.must.Matchers
import org.scalatest.wordspec.AnyWordSpec

class StateAtoZSpec extends AnyWordSpec with Matchers {
  "Manual State" should {
    final case class State(value: Int) {
      def flatMap(f: Int => State): State =
        f(value)

      def map(f: Int => Int): State =
        State(f(value))
    }

    "be applied" in {
      val result: State =
        for {
          a <- State(20)
          b <- State(a + 15) // Manually carry over state
          c <- State(b + 0)  // Manually carry over state
        } yield c

      result mustBe State(35)
    }
  }

  "Automatic Custom State" should {
    final case class State[S, A](run: S => (S, A)) {
      def flatMap[B](f: A => State[S, B]): State[S, B] =
        State { (s0: S) =>
          val (s1, a) = run(s0)
          f(a) run s1
        }

      def map[B](f: A => B): State[S, B] =
        flatMap(a => State.point(f(a)))
    }

    object State {
      def point[S, A](a: A): State[S, A] =
        State(s => (s, a))
    }

    "be applied" in {
      final case class GolfState(distance: Int)

      def swing(distance: Int): State[GolfState, Int] =
        State { (s0: GolfState) =>
          val nextDistance = s0.distance + distance
          (GolfState(nextDistance), nextDistance)
        }

      val resultEffect: State[GolfState, Int] =
        for {
          _ <- swing(20)
          _ <- swing(15)
          total <- swing(0)
        } yield total

      val result: (GolfState, Int) =
        resultEffect run GolfState(0) // Give initial "state" to "run" which executes the effect much like "unsafeRunSync" from Cats IO

      result._1 mustBe GolfState(35)
      result._2 mustBe 35
    }
  }
}