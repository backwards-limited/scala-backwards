package tech.backwards.essentialeffects

import cats.effect._
import cats.implicits._
import scala.concurrent.duration._
import tech.backwards.essentialeffects.debug._

object BeepWhen13 extends IOApp {
  def run(args: List[String]): IO[ExitCode] =
    for {
      ticks <- Ref[IO].of(0L)
      // We create a Deferred that will hold a Unit value once the condition is met.
      is13 <- Deferred[IO, Unit]
      // The two effects are only communicating through the shared is13 value.
      _ <- (beepWhen13(is13), tickingClock(ticks, is13)).parTupled
    } yield ExitCode.Success

  def beepWhen13(is13: Deferred[IO, Unit]): IO[Unit] =
    for {
      // Calling get will block the current effect until is13 has a value. (Fortunately the thread is released)
      _ <- is13.get
      _ <- IO("BEEP!").debug
    } yield ()

  def tickingClock(ticks: Ref[IO, Long], is13: Deferred[IO, Unit]): IO[Unit] =
    for {
      _ <- IO.sleep(1.second)
      _ <- IO(System.currentTimeMillis).debug
      count <- ticks.updateAndGet(_ + 1)
      // tickingClock is responsible for evaluating the condition we’re interested in.
      // If ticks has value 13, it calls complete to provide a value to the Deferred, unblocking any waiting effects.
      _ <- if (count >= 13) is13.complete(()) else IO.unit
      _ <- tickingClock(ticks, is13)
    } yield ()
}

// Previously, without Deferred, we would have to do something like:
object BeepWhen13WithoutUsingDeferred extends IOApp {
  def run(args: List[String]): IO[ExitCode] =
    for {
      ticks <- Ref[IO].of(0L)
      // We pass the Ref that holds our counter state into beepWhen13 since we don't have a Deferred.
      // Now we will have to poll the counter, which is BAD, as we have to guess a good enough polling rate.
      _ <- (beepWhen13(ticks), tickingClock(ticks)).parTupled
    } yield ExitCode.Success

  def beepWhen13(ticks: Ref[IO, Long]): IO[Unit] =
    for {
      t <- ticks.get
      _ <- if (t >= 13) IO("BEEP!").debug
      else IO.sleep(1.seconds) *> beepWhen13(ticks)
    } yield ()

  def tickingClock(ticks: Ref[IO, Long]): IO[Unit] =
    for {
      _ <- IO.sleep(1.second)
      _ <- IO(System.currentTimeMillis).debug
      count <- ticks.updateAndGet(_ + 1)
      _ <- tickingClock(ticks)
    } yield ()
}