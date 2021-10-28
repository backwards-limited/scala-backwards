package com.backwards.fp.monad

import cats.effect
import cats.effect.{ExitCode, IOApp}
import com.backwards.fp.monad.MonadTransformerAtoZ._

object MonadTransformerAtoZ {
  /**
   * MONAD
   */
  trait Monad[M[_]] {
    def point[A](a: => A): M[A]

    def flatMap[A, B](ma: M[A])(f: A => M[B]): M[B]

    def map[A, B](ma: M[A])(f: A => B): M[B] =
      flatMap(ma)(a => point(f(a)))
  }

  /**
   * IO
   */
  class IO[A] private(run: => A) {
    def unsafeRunAsync: A = run

    def flatMap[B](f: A => IO[B]): IO[B] = {
      val ioB: IO[B] =
        f(unsafeRunAsync)

      IO(ioB.unsafeRunAsync)
    }

    def map[B](f: A => B): IO[B] =
      flatMap(a => IO(f(a)))
  }

  object IO {
    def apply[A](a: => A): IO[A] =
      new IO(a)
  }

  /**
   * STATE
   */
  final case class State[S, A](run: S => (S, A)) {
    // This code is like: `xs.filter { i : Int => i < 3 }`
    def flatMap[B](g: A => State[S, B]): State[S, B] =
      State { (s0: S) =>
        // Create a new (state, value) by applying `run` to the initial state, `s0`. As shown above, `run` transforms an S to an (S, A).
        val (s1, a) = run(s0)   // (S, A) <~~ the type `S` here, not a State

        // Create a new State by applying `g` to `a`. as shown above, `g` transforms an A to a `State[S, B]`.
        val s2 = g(a) // State[S, B]

        // Create a final result by applying s2.run to s1. Once again, `run` transforms an S to an (S, A).
        val rez = s2.run(s1) // (S, B)

        // Yield this value; it is the param that's passed to State's constructor, so this function yields a State[S, B] as its final result
        rez
      }

    def map[B](f: A => B): State[S, B] =
      flatMap(a => State.point(f(a)))
  }

  object State {
    def point[S, A](a: A): State[S, A] =
      State(run = s => (s, a))
  }

  /**
   * STATE TRANSFORMER
   */
  case class StateT[M[_]: Monad, S, A](run: S => M[(S, A)]) {
    def flatMap[B](g: A => StateT[M, S, B]): StateT[M, S, B] =
      StateT { (s0: S) =>
        implicitly[Monad[M]].flatMap(run(s0)) {
          case (s1, a) => g(a).run(s1)
        }
      }

    def map[B](f: A => B): StateT[M, S, B] =
      flatMap(a => StateT.point(f(a)))
  }

  object StateT {
    def point[M[_]: Monad, S, A](a: A): StateT[M, S, A] =
      StateT(s => implicitly[Monad[M]].point((s, a)))
  }
}

object ForExpression extends App {
  implicit val IOMonad: Monad[IO] =
    new Monad[IO] {
      def point[A](a: => A): IO[A] =
        IO(a)

      def flatMap[A, B](ma: IO[A])(f: A => IO[B]): IO[B] =
        ma.flatMap(f)
    }

  final case class IntState(i: Int)

  def add(i: Int): StateT[IO, IntState, Int] =
    StateT[IO, IntState, Int] { oldState =>
      val newValue = i + oldState.i
      val newState = oldState.copy(i = newValue)
      IO(newState, newValue)
    }

  def multiply(i: Int): StateT[IO, IntState, Int] =
    StateT[IO, IntState, Int] { oldState =>
      val newValue = i * oldState.i
      val newState = oldState.copy(i = newValue)
      IO(newState, newValue)
    }

  val a: StateT[IO, IntState, Int] =
    add(1)

  val b: IO[(IntState, Int)] =
    a.run(IntState(1))

  b.map(t => println(s"b state = ${t._1}"))
  // prints “b state = IntState(2)”

  // A hard-coded example
  val forExpression: StateT[IO, IntState, Int] =
    for {
      _ <- add(2)        // 3
      _ <- add(3)        // 6
      x <- multiply(10)  // 6 x 10
    } yield x

  // “run” the state
  val result: IO[(IntState, Int)] =
    forExpression.run(IntState(1))

  // print the final state
  result.map(tuple => println(s"IntState = ${tuple._1}"))
}

object LoopWithQuitIncludingDebugging extends IOApp {
  def run(args: List[String]): effect.IO[ExitCode] =
    effect.IO {
      val result =
        sumLoop.run(SumState(0)).unsafeRunAsync

      println(s"Final SumState: $result")
    } as ExitCode.Success

  // The `IO` functions used previously
  def getLine: IO[String] = IO {
    val in = scala.io.StdIn.readLine()
    // println(s"getLine: in = |$in|")
    in
  }

  def putStr(s: String): IO[Unit] = IO {
    // println(s"putStr: s = |$s|")
    print(s)
  }

  def toInt(s: String): Int = {
    // println("toInt: s = $s")
    try {
      s.toInt
    } catch {
      case e: NumberFormatException => 0
    }
  }

  // A class to track the sum of the ints that are given
  final case class SumState(sum: Int)

  // An implementation of the `Monad` trait for the `IO` type
  implicit val IOMonad: Monad[IO] =
    new Monad[IO] {
      def point[A](a: => A): IO[A] = {
        // Only see this output when the loop exits properly
        // println(s"IOMonad::lift received 'a': $a")
        IO(a)
      }

      def flatMap[A, B](ma: IO[A])(f: A => IO[B]): IO[B] =
        ma.flatMap(f)
    }

  /**
   * Given the int `i`, add it to the previous `sum` from the given SumState `s`;
   * then return a new state `newState`, created with the new sum;
   * at the end of the function, wrap `newState` in an `IO`;
   * the anonymous function creates a `StateT` wrapped around that `IO`.
   */
  def doSumWithStateT(newValue: Int): StateT[IO, SumState, Int] =
    StateT { (oldState: SumState) =>
      // Create a new sum from `i` and the previous sum from `s`
      val newSum = newValue + oldState.sum
      println(s"updateIntState, old sum:   " + oldState.sum)
      println(s"updateIntState, new input: " + newValue)
      println(s"updateIntState, new sum:   " + newSum)

      // Create a new SumState
      val newState: SumState =
        oldState.copy(sum = newSum)

      // Return the new state and the new sum, wrapped in an IO
      IO(newState, newSum)
    }

  /**
   * The purpose of this function is to “lift” an IO action into the StateT monad.
   * given an IO instance named `io` as input, the anonymous function transforms
   * the `IO[A]` into an `IO[(SumState, A)]`; that result is then wrapped in a `StateT`.
   */
  def liftIoIntoStateT[A](io: IO[A]): StateT[IO, SumState, A] =
    StateT { s =>
      // Transform `IO[A]` into `IO(SumState, A)`
      val result: IO[(SumState, A)] =
        io.map(a => (s, a))

      // Debug: use this as a way to see what's going on here. if you enter 1 and then 2 you'll see the output, `(SumState(1), 2)`.
      // result.map(tup => println(s"lift: (${tup._1}, ${tup._2})"))

      // Yield the result of this anonymous function (which will be wrapped by StateT)
      result
    }

  // New versions of the i/o functions that use StateT
  def getLineAsStateT: StateT[IO, SumState, String] =
    liftIoIntoStateT(getLine)

  def putStrAsStateT(s: String): StateT[IO, SumState, Unit] =
    liftIoIntoStateT(putStr(s))

  /**
   * This loop stops when you type 'q' at the command line
   */
  def sumLoop: StateT[IO, SumState, Unit] =
    for {
      _     <- putStrAsStateT("\nGive me an int, or 'q' to quit: ")
      input <- getLineAsStateT
      _     <- if (input == "q") {
                  liftIoIntoStateT(IO(())) // quit
                } else for {
                  i <- liftIoIntoStateT(IO(toInt(input)))
                  _ <- doSumWithStateT(i)
                  _ <- sumLoop
                } yield ()
    } yield ()
}