package com.backwards.essentialeffects

import cats.effect._
import com.backwards.essentialeffects.debug._

/**
 * IO.async takes a function that provides a callback to report the asynchronous result.
 * So if we never want to return a result, we simply don’t invoke the callback!
 */
object Never extends IOApp {
  def run(args: List[String]): IO[ExitCode] =
    never
      .guarantee(IO("i guess never is now").debug.void)
      .as(ExitCode.Success)

  val never: IO[Nothing] =
    IO.async(_ => IO(None))
}