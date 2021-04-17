package com.backwards.essentialeffects

import java.util.concurrent.TimeUnit.MILLISECONDS
import scala.concurrent.duration.FiniteDuration

object Timing extends App {
  val clock: MyIO[Long] =
    MyIO(() => System.currentTimeMillis())

  def time[A](action: MyIO[A]): MyIO[(FiniteDuration, A)] =
    clock.flatMap(start =>
      action.flatMap(a =>
        clock.map(end =>
          new FiniteDuration(end - start, MILLISECONDS) -> a
        )
      )
    )

  val timedHello: MyIO[(FiniteDuration, Unit)] =
    time(MyIO.putStr("hello"))

  timedHello.unsafeRun() match {
    case (duration, _) => println(s"'hello' took $duration")
  }
}