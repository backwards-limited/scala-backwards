package tech.backwards.fp

object Function extends Function

trait Function {
  object syntax {
    implicit class Pipe[A](a: A) {
      def |>[B](f: A => B): B =
        f(a)
    }
  }
}