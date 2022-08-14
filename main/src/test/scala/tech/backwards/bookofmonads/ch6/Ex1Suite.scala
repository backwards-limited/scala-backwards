package tech.backwards.bookofmonads.ch6

import cats.implicits.{catsSyntaxFlatMapOps, catsSyntaxOptionId, none}
import cats.{Functor, Monad}
import munit.FunSuite

/**
 * Pure Reader-Writer-State Monads.
 *
 * Reader and Writer provide just one part of a stateful interface, obtaining or saving a value, respectively.
 * Another common characteristic among these three monads is that they are built from simple, functional building blocks: functions and tuples.
 */
class Ex1Suite extends FunSuite {
  /**
   * A value of `State s a` should be understood as a computation that produces values of type `a` while modifying an internal state of types.
   * In fact, we can see `State s a` as the encoding of functions from an initial state to their return values, paired with the new state:
   * {{{
   *   type State s a = s -> (a, s)
   * }}}
   *
   * Due to restrictions in the way type class instances in Haskell are resolved, it is not possible to write a Monad instance for a type synonym.
   * So instead it is better to define State as:
   * {{{
   *   newtype State s a = State { runState :: s -> (a, s) }
   * }}}
   */
  test("State Monad - Our own implementation") {
    final case class State[S, A](run: S => (S, A))

    def functorState[S]: Functor[State[S, *]] =
      new Functor[State[S, *]] {
        def map[A, B](Fa: State[S, A])(f: A => B): State[S, B] =
          State { s =>
            val (nextS, a) = Fa.run(s)
            (nextS, f(a))
          }
      }

    val s1: State[Int, String] =
      State(s => (s + 1, s" [S1: $s] "))

    val s2: State[Int, String] =
      functorState.map(s1)(str => s" [S2: $str] ")

    val s3: State[Int, String] =
      functorState.map(s2)(str => s" [S3: $str] ")

    println(s3.run(5))

    def monadState[S]: Monad[State[S, *]] =
      new Monad[State[S, *]] {
        def pure[A](a: A): State[S, A] =
          State(s => (s, a))

        def flatMap[A, B](Fa: State[S, A])(f: A => State[S, B]): State[S, B] =
          State { s =>
            val (nextS, a) = Fa.run(s)
            f(a).run(nextS)
          }

        def tailRecM[A, B](a: A)(f: A => State[S, A Either B]): State[S, B] =
          State { s =>
            f(a).run(s) match {
              case (nextS, Right(b)) =>
                (nextS, b)
              case (nextS, Left(a)) =>
                tailRecM(a)(f).run(nextS)
            }
          }
      }

    val states: State[Int, String] =
      monadState.flatMap(s1)(str1 =>
        monadState.flatMap(s2)(str2 =>
          monadState.map(s3)(str3 =>
            // str1 = [S1: 5]
            // str2 = [S2:  [S1: 6] ]
            // str3 = [S3:  [S2:  [S1: 7] ] ]
            str1 + str2 + str3
            // [S1: 5]  [S2:  [S1: 6] ]  [S3:  [S2:  [S1: 7] ] ]
          )
        )
      )

    println(states.run(5))
  }
}