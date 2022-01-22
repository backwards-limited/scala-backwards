package tech.backwards.fp.demo.writer

import cats.data.Writer

/**
 * A monad is a mechanism for sequencing computations.
 * Monads provide a pattern for abstracting over the effects of these computations, allowing them to be composed into larger programs.
 *
 * Writer Monad:
 * Provides a means to return a log along with the result of a computation.
 */
object WriterDemo extends App {
  val x = Writer(Vector("some intermediary computation"), 3)
  val y = Writer(Vector("another intermediary computation"), 4)

  val z = for {
    a <- x
    b <- y
  } yield a + b

  println(z) // WriterT((Vector(some intermediary computation, another intermediary computation), 7))

  println(z.value) // Id[Int] = 7

  println(z.written) // Id[Vector[String]] = Vector(some intermediary computation, another intermediary computation)

  val (log: Vector[String], result: Int) = z.run


  import cats.syntax.applicative._
  import cats.syntax.writer._ // "tell" and "writer"

  type Logged[A] = Writer[Vector[String], A]

  val writer = for {
    a <- 10.pure[Logged] // No log
    _ <- Vector("a", "b", "c").tell // No value, but log still gets appended
    b <- 32.writer(Vector("x", "y", "z")) // Log and value
  } yield a + b // map transforms the result

  println(writer) // WriterT((Vector(a, b, c, x, y, z), 42))
}