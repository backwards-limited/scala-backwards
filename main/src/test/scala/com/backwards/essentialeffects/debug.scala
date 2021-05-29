package com.backwards.essentialeffects

import cats.effect._
import cats.implicits._

/**
 * `import com.backwards.essentialeffects.debug._` to access the `debug` extension methods.
 */
object debug {
  /** Extension methods for an effect of type `F[A]`. */
  implicit class DebugHelper[A](ioa: IO[A]) {
    /** Print to the console the value of the effect along with the thread it was computed on. */
    def debug: IO[A] =
      ioa.flatTap { a =>
        val tn = Thread.currentThread.getName
        IO(println(s"[${Colorize.reversed(tn)}] $a"))
      }
  }
}