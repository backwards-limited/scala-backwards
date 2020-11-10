package com.backwards.fp.effects.iteration1

import scala.util.{Failure, Success, Try}

/**
 * https://medium.com/wix-engineering/demystifying-functional-effect-systems-in-scala-14419039a423
 *
 * Creating and combining effects.
 *
 * Effect systems are usually based on the concept of IO Monad,
 * which is a construct, wrapping a zero argument function that may execute an effect and produce a value.
 *
 * A basic IO monad might be defined as the following trait,
 * with the effect constructor (also commonly called pure) on the companion object, for creating actual concrete instances
 */
trait IO[+A] {
  def flatMap[B](f: A => IO[B]): IO[B]

  def map[B](f: A => B): IO[B]

  /** run the effect */
  def run(): A
}

object IO {
  def effect[A](a: => A): IO[A] = ???
}

object BasicApp extends App {
  // Using flatMap we can construct arbitrarily complex programs out of simple IO wrapped effects (here flatMap is hidden behind the syntactic sugar of our for comprehension):
  val program: IO[String] = for {
    _    <- IO.effect(println("What's your name?"))
    name <- IO.effect(scala.Console.in.readLine)
    _    <- IO.effect(println(s"Hello $name!"))
  } yield name

  program.run()
}

/**
 * We represent our basic operations as case classes, so each instance of TIO is an immutable value describing the effect to run.
 * Such descriptions are sometimes called an Algebra.
 *
 * We also move the concern of running the effects into our Runtime (also called an interpreter).
 * This allows a clean separation of describing programs as compositions of TIO s and the implementation details of running the composed programs.
 * As Runtime becomes more complex, we'll likely want to make it configurable, or even support multiple runtimes.
 */
sealed trait TIO[+A] {
  def flatMap[B](f: A => TIO[B]): TIO[B] = TIO.FlatMap(this, f)

  def map[B](f: A => B): TIO[B] = flatMap(a => TIO.succeed(f(a)))

  /** A convenience operator for sequencing effects, where the result of the first effect is ignored */
  def *> [B](that: TIO[B]): TIO[B] = flatMap(_ => that)
}

object TIO {
  /** Effect constructor */
  case class Effect[+A](a: () => A) extends TIO[A]

  /** Effect combinator */
  case class FlatMap[A, B](tio: TIO[A], f: A => TIO[B]) extends TIO[B]

  // Constructor functions
  def succeed[A](a: A): TIO[A] = Effect(() => a)

  def effect[A](a: => A): TIO[A] = Effect(() => a)
}

trait Runtime {
  /** Run tio and return Success/Failure */
  def unsafeRunSync[A](tio: TIO[A]): Try[A]
}

/**
 * Implement the interpreter using simple recursion
 */
object Runtime extends Runtime {
  def unsafeRunSync[A](tio: TIO[A]): Try[A] = eval(tio)

  def eval[A](tio: TIO[A]): Try[A] = tio match {
    case TIO.Effect(a) =>
      Try(a())

    case TIO.FlatMap(tio, f: (Any => TIO[Any])) =>
      eval[Any](tio) match {
        case Success(res) => eval(f(res))
        case Failure(e) => Failure(e)
      }
  }
}

/**
 * So, how do we use our TIO?
 * Lets define a helper trait TIOApp for apps that run TIO.
 */
trait TIOApp {
  def run: TIO[Any]

  final def main(args: Array[String]): Unit = {
    val whoops: Throwable => Nothing = { t =>
      println("Whoops, going to proxy throwing an error")
      throw t
    }

    Runtime.unsafeRunSync(run).fold(whoops, identity)
  }
}

/**
 * Now to run a TIO effect
 */
object SequenceEffects extends TIOApp {
  def run: TIO[Unit] =
    for {
      _ <- TIO.effect(println("Running first effect"))
      _ <- TIO.effect(println("Running second effect"))
    } yield ()
}

/**
 * To reduce boiler plate, lets introduce a TIO friendly print function.
 */
object Console {
  def putStrLn(str: => String): TIO[Unit] = TIO.effect(println(str))
}

import Console._

/**
 * Of course the code lifted into TIO.effect can throw an exception, let's see what happens then:
 */
object ExampleWithThrow extends TIOApp {
  override def run: TIO[Unit] =
    for {
      _ <- putStrLn("Running first effect")
      _ <- TIO.effect(throw new RuntimeException)
      _ <- putStrLn("Running second effect")
    } yield ()
}