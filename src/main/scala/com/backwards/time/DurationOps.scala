package com.backwards.time

import scala.concurrent.duration.Duration

object DurationOps extends DurationOps

trait DurationOps {
  implicit val duration2Java: Duration => java.time.Duration =
    d => java.time.Duration.ofMillis(d.toMillis)
}