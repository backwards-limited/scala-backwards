package com.backwards.fs2

import java.nio.file.Paths
import java.util.concurrent.Executors
import scala.concurrent.ExecutionContext
import cats.effect.internals.IOContextShift
import cats.effect.{ContextShift, ExitCode, IO}
import cats.implicits._
import fs2.{io, text}
import org.scalatest.{FreeSpec, MustMatchers}

/**
  * [[https://medium.freecodecamp.org/a-streaming-library-with-a-superpower-fs2-and-functional-programming-6f602079f70a FS2 Article]]
  *
  * FS2 has one central type: Stream[Effect, Output]
  * It's a Stream and that it emits values of type Output.
  * But what is Effect? What is the link between Effect and Output?
  */
class FS2Spec extends FreeSpec with MustMatchers {
  """"What problems can I solve with FS2?"""- {
    """
       - Streaming I/O: Loading incrementally big data sets that would not fit in memory
         and operating on them without blowing your heap.

       - Control Flow: Moving data from one/several DBs/files/APIs to others in a nice declarative way.

       - Concurrency: Run different streams in parallel and make them communicate together.
         For example loading data from multiple files and processing them concurrently as opposed to sequentially.
         You can do some advanced stuff here.
         Streams can communicate together during the processing stage and not only at the end.
    """ in {}
  }

  """What can I do with Stream that List cannot?""" - {
    """Your data is too big to fit into memory""" - {
      """
        Let’s say you have a very big file (40GB) fahrenheit.txt.
        The file has a temperature on each line and you want to convert it to celsius.txt.

        import scala.io.Source

        val list = Source.fromFile("src/test/resources/fahrenheit.txt").getLines.toList
        java.lang.OutOfMemoryError: Java heap space
      """ in {
        implicit def contextShift: ContextShift[IO] =
          IOContextShift.global

        def fahrenheitToCelsius(f: Double): Double =
          (f - 32.0) * (5.0 / 9.0)

        val blockingExecutionContext = ExecutionContext.fromExecutorService(Executors.newFixedThreadPool(2))

        val task = io.file.readAll[IO](Paths.get("src/test/resources/fahrenheit.txt"), blockingExecutionContext, 4096)
          .through(text.utf8Decode)
          .through(text.lines)
          .filter(s => !s.trim.isEmpty && !s.startsWith("//"))
          .map(line => fahrenheitToCelsius(line.toDouble).toString)
          .intersperse("\n")
          .through(text.utf8Encode)
          .through(io.file.writeAll(Paths.get("src/test/resources/celsius.txt"), blockingExecutionContext))
          .compile.drain
          .as(ExitCode.Success)

        task.unsafeRunSync() mustBe ExitCode.Success
      }
    }
  }

  """How can I feed data from an API/File/DB to Stream?""" - {

  }

  """What is this Effect type and how does it relate to functional programming?""" - {

  }
}