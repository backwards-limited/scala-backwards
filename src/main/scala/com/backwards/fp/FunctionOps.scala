package com.backwards.fp

trait FunctionOps {
  implicit class Pipe[A](a: A) {
    def |>[B](f: A => B): B = f(a)
  }
}

object FunctionOps extends FunctionOps