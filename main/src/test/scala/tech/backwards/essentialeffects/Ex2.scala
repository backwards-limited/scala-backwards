package tech.backwards.essentialeffects

import java.time.LocalTime
import java.time.temporal.ChronoUnit
import scala.concurrent.duration._
import cats.effect._
import cats.implicits._
import tech.backwards.essentialeffects.debug._

object TickingClock extends IOApp {
  val tickingClock: IO[Unit] =
    IO(println(LocalTime.now().truncatedTo(ChronoUnit.SECONDS))).debug >> IO.sleep(1 second) >> tickingClock

  def run(args: List[String]): IO[ExitCode] =
    tickingClock.as(ExitCode.Success)
}