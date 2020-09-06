package com.backwards.fp.effects.iteration4

import scala.concurrent.{Await, Future, Promise}
import scala.concurrent.duration._
import scala.util.control.NonFatal
import scala.util.{Failure, Success, Try}
import java.time.Instant
import java.util.{Timer, TimerTask}
import java.util.concurrent.atomic.AtomicInteger
import java.util.concurrent._
import java.util.concurrent.Executors._

/**
 * Asynchrony - We want to support asynchronous effects.
 * Effects that are not bound to the thread that may have initiated them. This is essential for supporting non-blocking operations, such as IO.
 *
 * A common way to express such asynchronous processes is by using callbacks.
 * Lets change our Runtime API to async style by adding a new method unsafeRunAsync
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

  /** Effect async */
  type AsyncDoneCallback[T] = Try[T] => Unit

  type AsyncTask[T] = AsyncDoneCallback[T] => Unit

  case class EffectAsync[A](asyncTask: AsyncTask[A]) extends TIO[A]

  // Constructor functions
  def succeed[A](a: A): TIO[A] = Effect(() => a)

  def effect[A](a: => A): TIO[A] = Effect(() => a)

  def fail[A](throwable: Throwable): TIO[A] = Fail(throwable)

  def effectAsync[A](asyncTask: AsyncTask[A]): TIO[A] = EffectAsync(asyncTask)

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
  /** Now unsafeRunSync can be implemented using unsafeRunAsync (for example, using Scala Promise and Future) */
  def unsafeRunAsync[A](tio: TIO[A])(callback: Try[A] => Unit): Unit

  def unsafeRunSync[A](tio: TIO[A], timeout: Duration = Duration.Inf): Try[A] =
    Await.ready(unsafeRunToFuture(tio), timeout).value.get

  def unsafeRunToFuture[A](tio: TIO[A]): Future[A] = {
    val promise = Promise[A]()
    unsafeRunAsync(tio)(promise.tryComplete)
    promise.future
  }
}

/**
 * Implement the interpreter using simple recursion
 */
object Runtime extends Runtime {
  //>>> Default Executor with 16 named threads
  val executor: Executor = Executor.fixed(16, "tio-default")

  override def unsafeRunAsync[A](tio: TIO[A])(callback: Try[A] => Unit): Unit =
    eval(tio)(callback.asInstanceOf[Try[Any] => Unit])

  def eval(tio: TIO[Any])(done: Try[Any] => Unit): Unit =
    //>>> Run the effect on the executor
    executor.submit {
      tio match {
        case TIO.Effect(a) =>
          done(Try(a()))

        case TIO.EffectAsync(callback) => callback(done)

        case TIO.FlatMap(tio, f) =>
          val fAny = f.asInstanceOf[Any => TIO[Any]]
          eval(tio) {
            case Success(res) => eval(fAny(res))(done)
            case Failure(e) => done(Failure(e))
          }
        case TIO.Fail(e) => done(Failure(e))

        case TIO.Recover(tio, f) =>
          eval(tio) {
            case Failure(e) => eval(f(e))(done)
            case success => done(success)
          }

        case TIO.EffectAsync(callback) =>
          callback(done)
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

object Clock {
  // Use EffectAsync to implement a non-blocking "sleep"
  val timer = new Timer("TIO-Timer", /* isDaemon */ true)

  def sleep[A](duration: Duration): TIO[Unit] =
    TIO.effectAsync[Unit] { onComplete =>
      timer.schedule(new TimerTask {
        override def run(): Unit = onComplete(Success(()))
      }, duration.toMillis)
    }
}

/**
 * Which allows us to write code like this:
 *
 * where
 * The first ran on the main thread,
 * and now our execution is truly asynchronous and our previous example with sleep prints.
 */
object SleepExample extends TIOApp {
  import Clock._

  def run: TIO[Unit] =
    for {
      _ <- TIO.effect(println(s"[${Instant.now}] Running first effect on ${Thread.currentThread.getName}"))
      _ <- sleep(2.seconds)
      _ <- TIO.effect(println(s"[${Instant.now}] Running second effect on ${Thread.currentThread.getName}"))
    } yield ()
}

/**
 * What we need is to queue our effects and execute them on worker thread(s).
 *
 * Lets define a convenient API for queueing work for execution — Executor, implemented using java executors:
 */
trait Executor {
  final def submit(thunk: => Unit): Unit = submitRunnable(() => thunk)

  def submitRunnable(thunk: Runnable): Unit
}

object Executor {
  val threadCounter = new AtomicInteger(0)

  def nextThreadId: Int = threadCounter.incrementAndGet()

  // Creates Executor from a fixed thread pool, with named threads
  def fixed(threads: Int, namePrefix: String): Executor = {
    val executor = newFixedThreadPool(threads, namedDaemonThreads(namePrefix))
    thunk => executor.submit(thunk)
  }

  def namedDaemonThreads(namePrefix: String): ThreadFactory = { thunk =>
    val thread = new Thread(thunk, s"$namePrefix-$nextThreadId")
    thread.setDaemon(true)
    thread.setUncaughtExceptionHandler((_, e) => e.printStackTrace())
    thread
  }
}