package com.backwards.fp.demo.state

import cats.data.State

/**
 * A monad is a mechanism for sequencing computations.
 * Monads provide a pattern for abstracting over the effects of these computations, allowing them to be composed into larger programs.
 *
 * State Monad:
 * Programs that carry state along with a computation can be expressed in terms of the State monad.
 * For some state S and result type A, the type State[S, A] represents a function that takes an initial state and returns a result together with some new state.
 * In the example below, we will show how a sequence of arithmetic operations can be chained together, passing the result as the state between each step.
 */
object StateDemo extends App {
  def increment: State[Int, String] = State[Int, String] { state =>
    val a = state + 1
    (a, s"Result of increment is $a")
  }

  def double: State[Int, String] = State[Int, String] { state =>
    val a = state * 2
    (a, s"Result of double is $a")
  }

  def modTen: State[Int, String] = State[Int, String] { state =>
    val a = state % 10
    (a, s"Result of modTen is $a")
  }

  // Because each of these individual steps is wrapped in a monad, we can chain them together using a for-comprehension

  def genNumber: State[Int, String] /*IndexedStateT[Eval, Int, Int, String]*/ = for {
    a <- increment  // Threads the new state to the next computation
    b <- double     // Threads the new state to the next computation
    c <- modTen
  } yield c

  // The resulting program can then be executed using the run method, passing in an initial state

  val (state, result) = genNumber.run(3).value

  println(state)  // 8
  println(result) // Result of modTen is 8

  // We can use the runA and runS methods to return only the result or state respectively

  val stateOnly  = genNumber.runS(3) // 8
  val resultOnly = genNumber.runA(3) // Result of modTen is 8

  println(stateOnly.value)  // Eval[Int] = 8
  println(resultOnly.value) // Eval[String] = Result of modTen is 8



  // How does the "threading of state" work?

  // Stateful computation: `S => (S, A)` where `S` is `List[A]`
  def head[A](xs: List[A]): (List[A], A) =
    (xs.tail, xs.head)

  val s: State[List[Int], Map[String, Int]] /*IndexedStateT[Eval, List[Int], List[Int], Map[String, Int]]*/ = for {
    a <- State(head[Int])
    b <- State(head[Int])
  } yield Map(
    "a" -> a,
    "b" -> b
  )

  val (sFinal, resultFinal) = s.run(List(1, 2, 3, 4, 5)).value

  println(sFinal)         // List(3, 4, 5)
  println(resultFinal)    // Map(a -> 1, b -> 2)

  val sOther: State[List[Int], Map[String, Int]] = State(head[Int]).flatMap { // flatMap calls "run" to thread through the state
    a => State(head[Int]).map { // map calls "run" to thread through the state
      b => Map("a" -> a, "b" -> b)
    }
  }
}