package com.backwards.fs2

import java.nio.file.Paths
import scala.concurrent.{ExecutionContext, Future}
import scala.util.Random
import cats.effect._
import cats.implicits._
import fs2._
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.must.Matchers

/**
  * [[https://medium.freecodecamp.org/a-streaming-library-with-a-superpower-fs2-and-functional-programming-6f602079f70a FS2 Article]]
  *
  * FS2 has one central type: Stream[Effect, Output]
  * It's a Stream and that it emits values of type Output.
  * But what is Effect? What is the link between Effect and Output?
  *
  * So, What problems can I solve with FS2?
  *
  * - Streaming I/O: Loading incrementally big data sets that would not fit in memory
  *   and operating on them without blowing your heap.
  *
  * - Control Flow: Moving data from one/several DBs/files/APIs to others in a nice declarative way.
  *
  * - Concurrency: Run different streams in parallel and make them communicate together.
  *   For example loading data from multiple files and processing them concurrently as opposed to sequentially.
  *   You can do some advanced stuff here.
  *   Streams can communicate together during the processing stage and not only at the end.
  */
class FS2Spec extends AnyFreeSpec with Matchers {
  """What can I do with Stream that List cannot?""" - {
    """
      Your data is too big to fit into memory

      Let’s say you have a very big file (40GB) fahrenheit.txt.
      The file has a temperature on each line and you want to convert it to celsius.txt.

      import scala.io.Source

      val list = Source.fromFile("src/test/resources/fahrenheit.txt").getLines.toList
      java.lang.OutOfMemoryError: Java heap space
    """ in {
      implicit val contextShift: ContextShift[IO] = IO.contextShift(ExecutionContext.global)

      val stream: Stream[IO, Unit] = Stream.resource(Blocker[IO]).flatMap { blocker =>
        def fahrenheitToCelsius(f: Double): Double =
          (f - 32.0) * (5.0 / 9.0)

        io.file.readAll[IO](Paths.get("src/test/resources/fahrenheit.txt"), blocker, 4096)
          .through(text.utf8Decode)
          .through(text.lines)
          .filter(s => s.trim.nonEmpty && !s.startsWith("//"))
          .map(line => fahrenheitToCelsius(line.toDouble).toString)
          .intersperse("\n")
          .through(text.utf8Encode)
          .through(io.file.writeAll(Paths.get("src/test/resources/celsius.txt"), blocker))
      }

      stream.compile.drain.as(ExitCode.Success).unsafeRunSync() mustBe ExitCode.Success
    }

    """
      Upon reading a file I want to write it back but preserve the line structure.
      I need to insert a newline character "\n" after each temperature.
      This can be achieved with the "intersperse" combinator.
    """ in {
      Stream(1, 2, 3, 4).intersperse("\n").toList mustBe List(1, "\n", 2, "\n", 3, "\n", 4)
    }

    """
      There is also "zipWithNext"
    """ in {
      Stream(1, 2, 3, 4).zipWithNext.toList mustBe List((1, Some(2)), (2, Some(3)), (3, Some(4)), (4, None))
    }
  }

  """How can I feed data from an API/File/DB to Stream?""" - {
    """
      Take a nonsensical simple program, for illustration purposes
    """ - {
      val program: Stream[Pure, Int] = Stream(1, 2, 3)

      """
        What does Pure mean? The source code is:

        type Pure[A] <: Nothing

        which indicates that a stream evaluates no effects,
        and a `Stream[Pure, O]` can be safely converted to a `Stream[F, O]` for all `F`.

        It means no effects..., but What is an effect?
        And more specifically what is the effect of our program Stream(1, 2, 3)?

        This program has literally no effect on the world. Its only effect will be to make your CPU work and consumes some power!!
        It does not affect the world around you.
        By affecting the world I mean it consumes any meaningful resource like a file, a database,
        OR it produces anything like a file, uploading some data somewhere, writing to your terminal, and so on.
      """ in {
        program.map(_ + 1).toList mustBe List(2, 3, 4) // No effects took place i.e. no interactions apart from applying a pure function
      }
    }
  }

  """What is this Effect type and how does it relate to functional programming?""" - {
    """
      How to turn a Pure Stream into something useful?
      Let's say I want to load user IDs from a DB, and we have a function that given a "user name" will call a database to retrieve the "user ID".
    """ - {
      // In memory Map representation of a database
      val database = Map("bob" -> 1, "alice" -> 2, "joe" -> 3)

      /**
        * This function returns a Future which indicates that this call is asynchronous and the value will be available at some point in the future.
        * It wraps the value returned by the DB.
        */
      def acquireUserId(userName: String)(implicit ec: ExecutionContext): Future[Int] = Future {
        // Pretend we call a database, but we'll just use an in memory Map representation.
        database(userName)
      }

      """
        Let's say we have the following Pure Stream and from it we wish to get a Stream of "user IDs".
        However, the following incorrectly uses "map".
      """ - {
        import scala.concurrent.ExecutionContext.Implicits.global

        val userNames: Stream[Pure, String] = Stream("bob", "alice", "joe")

        val userIdsWhoops: Stream[Pure, Future[Int]] = userNames.map(acquireUserId)

        """
          We still got back a Pure! I gave the Stream a function that affects the world and I still got a Pure, not cool ...
          It would have been neat if FS2 would have detected automatically that the "acquireUserId" function has an effect on the world
          and returned me something that is NOT Pure - but it does not work like that.
          You have to use a special combinator instead of map - you have to use evalMap.
        """ - {
          val userIds: Stream[Future, Int] = userNames.evalMap(acquireUserId)

          """
            The left side that was the Effect type parameter is now the concrete Future type.
            You just witnessed true separation of concerns.
            You can continue to operate on the stream with all the nice List like combinators and you don't have to worry about if the DB is down,
            slow or all the stuff that is related to the network (effect) concerns.
            It all works until I want to use toList to get the values back.
          """ in {
            "userIds.toList" mustNot compile
          }

          """
            What happened? I could swear that I used "toList" before and it worked, how can it say that toList is not a member of Stream[Future, Int] any more?
            Well, we need to "compile" to tell the Stream to combine all the effects into one, effectively it folds all the calls to "acquireUserId" into one big Future.
            Though the following still doesn't resolve the issue as Future is not a good Effect type.
            But remember that at this point, the DB still has not been called. Nothing happens really - the full program does not produce anything.
            Code that does not have an effect is called pure and that’s what all Functional Programming is about: writing programs with functions that are pure.
            Why would you want write code this way? Simple: to achieve separation of concerns between the IO parts and the rest of our code.
          """ in {
            "userIds.compile.toList" mustNot compile
          }

          """
            As already stated, Future is a bad Effect type, it goes against the separation of concerns principle.
            Indeed, Future is eager in Scala: the moment you create one it starts to executes on some thread, you don't have control of the execution and thus it breaks.
            FS2 is well aware of that and does not let you compile. To fix this we have to use a type called IO that wraps our bad Future.
          """ in {
            import com.backwards.transform.Transform._

            userIds.translate(`Future ~> IO`).compile.toList.unsafeRunSync() mustBe List(1, 2, 3)
          }

          "equivalent" in {
            implicit val contextShift: ContextShift[IO] = IO.contextShift(ExecutionContext.global)

            userNames.evalMap[IO, Int](name => IO fromFuture IO(acquireUserId(name))).compile.toList.unsafeRunSync() mustBe List(1, 2, 3)
          }

          """
            In the end we have:
            Stream[IO, Int]

            which states:
            This is a Stream that emits values of type Int and in order to do so, it needs to run an "effective function" that produces IO[Int] for each value.

            Use "evalMap" instead of "map" when you want to apply a function that has an effect like acquireUserId to a Stream.

            Stream[IO, Int] separates the concerns of "What" and "How" by letting you work only with the values and not worrying about how to get them (loading from the db).

            Separating program description from evaluation is a key aspect of FP.

            All the programs you write with Stream will do nothing until you use unsafeRunSync. Before that your code is effectively pure.

            IO[Int] is an effect type that tells you: you will get Long values from IO (could be a file, the network, the console ...). It's a description and not a wrapper!

            Finally, Future does not abide by this philosophy and thus is not compatible with FS2, you have to use IO type instead.
          """ - {}
        }
      }
    }
  }

  """Stream IO""" - {
    """Let's print""" in {
      val put: Int => IO[Unit] =
        s => IO(println(s))

      val stream: Stream[IO, Unit] = Stream(1, 2, 3).flatMap(x => Stream.repeatEval(put(x)).take(2))

      stream.compile.drain.unsafeRunSync()
    }
  }

  "Stream effects" - {
    "will be realised" in {
      val xs = Stream.eval(IO(Random.nextInt())).compile.toList
      println(xs.unsafeRunSync())

      val xsRepeat = Stream.repeatEval(IO(Random.nextInt())).take(5).compile.toList
      println(xsRepeat.unsafeRunSync())
    }
  }

  "Stream with pipes and sinks" - {
    "apply" in {
      val stream: Stream[IO, String] =
        Stream eval IO.pure("Hello World")

      val pipe: Pipe[IO, String, List[String]] =
        _.map(_.split(" ").toList)

      // val sink: Sink[IO, List[String]] = _.evalMap(xs => IO(xs.foreach(println))) <- Sink is deprecated
      val sink: Pipe[IO, List[String], Unit] =
        _.evalMap(xs => IO(xs.foreach(println)))

      val program: Stream[IO, Unit] =
        stream through pipe through sink

      program.compile.drain.unsafeRunSync()
    }
  }
}