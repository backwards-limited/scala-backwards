package tech.backwards.fp.demo.state

import cats.data.State
import cats.data.State.get

/**
 * https://www.joekearney.co.uk/posts/learning-about-state-monad
 *
 * Swap the top two elements of a stack.
 */
object StackStateDemo extends App {
  type S = List[Int] // Our stack type

  def push(i: Int): State[S, Unit] =
    State[S, Unit] { s =>
      (i +: s, ())
    }

  def pop: State[S, Int] =
    State[S, Int] {
      case i +: tail => (tail, i)
      case _ => sys.error("Whoops")
    }

  // Build up our program
  val program: State[S, S] /*IndexedStateT[Eval, S, S, S]*/ =
    for {
      i1 <- pop // State[S, Int]
      _ = println(s"i1 (Int) = $i1")
      i2 <- pop // State[S, Int]
      _ = println(s"i2 (Int) = $i2")
      _ <- push(i1) // State[S, Unit]
      _ <- push(i2) // State[S, Unit]
      s <- get // State[S, S]
    } yield s

  val result: S = program.runA(List(1, 2, 3, 4, 5)).value

  println(result)
}