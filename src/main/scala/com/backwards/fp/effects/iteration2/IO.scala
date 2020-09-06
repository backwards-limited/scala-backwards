package com.backwards.fp.effects.iteration2

import scala.util.control.NonFatal
import scala.util.{Failure, Success, Try}

/**
 * To support errors, we will introduce a proper way to fail (without lifting a throw statement into an effect) and a new combinator called recover.
 */
sealed trait TIO[+A] {
  def flatMap[B](f: A => TIO[B]): TIO[B] = TIO.FlatMap(this, f)

  def map[B](f: A => B): TIO[B] = flatMap(a => TIO.succeed(f(a)))

  /** A convenience operator for sequencing effects, where the result of the first effect is ignored */
  def *> [B](that: TIO[B]): TIO[B] = flatMap(_ => that)

  // >>> New recover combinator
  def recover[B >: A](f: Throwable => TIO[B]): TIO[B] = TIO.Recover(this, f)
}

object TIO {
  /** Effect constructor */
  case class Effect[+A](a: () => A) extends TIO[A]

  /** Effect combinator */
  case class FlatMap[A, B](tio: TIO[A], f: A => TIO[B]) extends TIO[B]

  case class Fail[A](e: Throwable) extends TIO[A]

  case class Recover[A](tio: TIO[A], f: Throwable => TIO[A]) extends TIO[A]

  // Constructor functions
  def succeed[A](a: A): TIO[A] = Effect(() => a)

  def effect[A](a: => A): TIO[A] = Effect(() => a)

  def fail[A](throwable: Throwable): TIO[A] = Fail(throwable)

  // Functions
  def foreach[A, B](xs: Iterable[A])(f: A => TIO[B]): TIO[Iterable[B]] =
    xs.foldLeft(TIO.succeed(Vector.empty[B]))(
      (acc, a) => for {
        soFar <- acc
        x <- f(a)
      } yield soFar :+ x
    )
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

    // >>> new error related effects:
    case TIO.Fail(t) =>
      Failure(t)

    case TIO.Recover(tio, f) =>
      eval(tio) match {
        case Failure(e) => eval(f(e))
        case success => success
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
 * To reduce boiler plate, lets introduce a TIO friendly print function.
 */
object Console {
  def putStrLn(str: => String): TIO[Unit] = TIO.effect(println(str))
}

import Console._

object FailAndRecover extends TIOApp {
  def run: TIO[Unit] =
    (for {
      _ <- putStrLn("Running first effect")
      _ <- TIO.fail(new RuntimeException)
      _ <- putStrLn("Second effect - will not run")
    } yield ()).recover {
      case NonFatal(e) =>
        putStrLn(s"Recovered from failure: ${e.getClass.getName}")
    }
}

/**
 * So far, our toy IO has at least two serious limitations:
 * - it is fully synchronous (everything runs in the thread that calls unsafeRunSync)
 * - it is not stack safe.
 */
object Foreach10k extends TIOApp {
  def run: TIO[Iterable[Unit]] = TIO.foreach(1 to 10000)(i => TIO.effect(println(i)))
}