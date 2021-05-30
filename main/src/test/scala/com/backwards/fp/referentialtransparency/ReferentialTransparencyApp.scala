package com.backwards.fp.referentialtransparency

import scala.io.StdIn
import cats.effect.IO
import cats.effect.unsafe.implicits.global
import cats.implicits._

object ReferentialTransparencyApp extends App {
  val read: IO[Int] = IO(StdIn.readInt())

  val result1: IO[Int] = (read, read).mapN(_ + _)
  println(result1.unsafeRunSync())

  val result2: IO[Int] = (IO(StdIn.readInt()), IO(StdIn.readInt())).mapN(_ + _)
  println(result2.unsafeRunSync())
}