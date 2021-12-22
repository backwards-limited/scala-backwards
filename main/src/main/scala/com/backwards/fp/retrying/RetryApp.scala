package com.backwards.fp.retrying

import scala.concurrent.duration._
import scala.util.Random
import cats.effect.{IO, IOApp}
import retry.{RetryDetails, RetryPolicies, RetryPolicy, retryingOnFailures, retryingOnSomeErrors}

/**
 * https://softwaremill.com/retrying-function-calls-in-scala/
 */
sealed trait RetryApp extends IOApp.Simple {
  /**
   * The onFailure function is an example of the callback function that will be called by cats-retry when the result obtained by calling our action is not what we expected.
   */
  def onFailure(failedValue: Int, details: RetryDetails): IO[Unit] =
    IO(println(s"Rolled a $failedValue, retrying ... $details"))

  /**
   * The onError is an example of the callback function, called when our proper action execution throws an exception wrapped in a MonadError instance.
   */
  def onError(err: Throwable, details: RetryDetails): IO[Unit] =
    IO(println(s"Recovering from ${err.getMessage}"))
}

object RetryOnFailuresApp extends RetryApp {
  /**
   * This policy defined is just a simple example of the policy to retry our action call every 1 second.
   */
  val policy: RetryPolicy[IO] =
    RetryPolicies.constantDelay[IO](1.second)

  val loadedDie: LoadedDie =
    LoadedDie(2, 5, 4, 1, 3, 2, 6).startAt(seq => (new Random).nextInt(seq.length - 1))

  val unsafeFunction: IO[Int] =
    IO(loadedDie.roll)

  val isResultOk: Int => IO[Boolean] =
    i => IO(i == 3)

  val program: IO[Int] =
    retryingOnFailures(policy, isResultOk, onFailure)(unsafeFunction)

  def run: IO[Unit] =
    program.map(r => println(s"Finished with: $r"))
}

object RetryLimitApp extends RetryApp {
  /**
   * This policy defined is just a simple example of the policy to retry our action call every 1 second.
   */
  val policy: RetryPolicy[IO] =
    RetryPolicies.limitRetries(3)

  val loadedDie: LoadedDie =
    LoadedDie(2, 5, 4, 1, 3, 2, 6)

  val unsafeFunction: IO[Int] =
    IO(loadedDie.roll)

  val isResultOk: Int => IO[Boolean] =
    i => IO(i == 3)

  val program: IO[Int] =
    retryingOnFailures(policy, isResultOk, onFailure)(unsafeFunction)

  def run: IO[Unit] =
    program.map(r => println(s"Finished with: $r"))
}

object CatsRetryOnSomeErrors extends RetryApp {
  val policy: RetryPolicy[IO] =
    RetryPolicies.constantDelay[IO](1.second)

  val loadedDie: LoadedDie =
    LoadedDie(2, 5, 4, 1, 3, 2, 6)

  // Interesting - Cannot be val
  def unsafeFunction: IO[Int] = {
    val res: Int =
      loadedDie.roll

    if (res != 4)
      IO.raiseError(new IllegalArgumentException("Roll different than 4"))
    else
      IO.pure(res)
  }

  val isIOException: Throwable => IO[Boolean] = {
    case _: IllegalArgumentException => IO.pure(true)
    case _ => IO.pure(false)
  }

  val program: IO[Int] =
    retryingOnSomeErrors(isWorthRetrying = isIOException, policy = policy, onError = onError)(unsafeFunction)

  override def run: IO[Unit] =
    program.map(r => println(s"Finished with: $r"))
}