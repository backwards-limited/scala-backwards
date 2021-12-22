package com.backwards.fp.retrying

import java.util.concurrent.atomic.AtomicInteger

final case class LoadedDie(rolls: Int*) {
  private val i: AtomicInteger =
    new AtomicInteger(-1)

  def startAt(f: Seq[Int] => Int): LoadedDie = {
    val start: Int =
      f(rolls)

    i.set(if (start >= rolls.length) 0 else start)

    this
  }

  def roll: Int = {
    rolls(i.updateAndGet { current =>
      val next: Int =
        current + 1

      if (next >= rolls.length) 0 else next
    })
  }
}