package com.backwards.essentialeffects

import java.time.LocalTime
import java.time.temporal.ChronoUnit
import cats.effect._
import cats.implicits._
import scala.concurrent.duration._

object TickingClock extends IOApp {
  val tickingClock: IO[Unit] =
    IO(println(LocalTime.now().truncatedTo(ChronoUnit.SECONDS))) >> IO.sleep(1 second) >> tickingClock

  def run(args: List[String]): IO[ExitCode] =
    tickingClock.as(ExitCode.Success)
}