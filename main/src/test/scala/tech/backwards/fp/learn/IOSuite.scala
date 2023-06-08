package tech.backwards.fp.learn

import java.lang.System.lineSeparator
import java.util.concurrent.atomic.{AtomicInteger, AtomicReference}
import munit.ScalaCheckSuite
import org.scalacheck.Test

class IOSuite extends ScalaCheckSuite {
  override protected def scalaCheckTestParameters: Test.Parameters =
    super.scalaCheckTestParameters.withMinSuccessfulTests(100).withMaxDiscardRatio(10)

  property("Run IO") {
    val io: IO[String] =
      IO("foo")

    assertEquals(io.unsafeRunSync(), "foo")
  }

  property("Run IO twice") {
    val callCount: AtomicInteger =
      new AtomicInteger(0)

    val io: IO[String] =
      IO {
        callCount.incrementAndGet()
        "foo"
      }

    assertEquals(io.unsafeRunSync(), "foo")
    assertEquals(callCount.get(), 1)

    assertEquals(io.unsafeRunSync(), "foo")
    assertEquals(callCount.get(), 2)
  }

  property("IO Functor") {
    val io: IO[String] =
      IO("foo")

    val mappedIO: IO[String] =
      Functor[IO].fmap(io)(_ + "bar")

    assertEquals(mappedIO.unsafeRunSync(), "foobar")
  }

  property("IO Functor syntax") {
    import tech.backwards.fp.learn.Functor.syntax._

    val io: IO[String] =
      IO("foo")

    val mappedIO: IO[String] =
      io `<$>` (_ + "bar")

    assertEquals(mappedIO.unsafeRunSync(), "foobar")
  }

  property("IO Monad") {
    val io: IO[String] =
      Monad[IO].pure("foo")

    val flatMappedIO: IO[String] =
      Monad[IO].flatMap(io)(s => IO(s + "bar"))

    assertEquals(flatMappedIO.unsafeRunSync(), "foobar")
  }

  property("IO Monad syntax") {
    import tech.backwards.fp.learn.Monad.syntax._

    val io: IO[String] =
      "foo".pure[IO]

    val flatMappedIO: IO[String] =
      io >>= (s => IO(s + "bar"))

    assertEquals(flatMappedIO.unsafeRunSync(), "foobar")
  }

  property("IO Applicative") {
    val io: IO[String] =
      Applicative[IO].pure("foo")

    val apedIO: IO[Int] =
      Applicative[IO].ap(IO((s: String) => s.length))(io)

    assertEquals(apedIO.unsafeRunSync(), 3)
  }

  property("IO simulate reading from a file twice with an update") {
    import tech.backwards.fp.learn.Functor.syntax._
    import tech.backwards.fp.learn.Monad.syntax._

    val readFile: StateT[IO, String, String] =
      StateT((s: String) => IO(s -> s))

    val appendFile: String => StateT[IO, String, Unit] =
      s => StateT.modify(_ + s)

    val program: StateT[IO, String, (String, String)] =
      for {
        originalContents <- readFile
        _ <- appendFile(s"bar$lineSeparator")
        latestConents <- readFile
      } yield originalContents -> latestConents

    val (latestContents1, (originalContents, latestContents2)) =
      program.run(s"foo$lineSeparator").unsafeRunSync()

    assertEquals(originalContents, s"foo$lineSeparator")
    assertEquals(latestContents1, s"foo${lineSeparator}bar$lineSeparator")
    assertEquals(latestContents1, latestContents2)
  }

  property("IO simulate reading from a file twice with an update") {
    import tech.backwards.fp.learn.Functor.syntax._
    import tech.backwards.fp.learn.Monad.syntax._

    val fileContents: AtomicReference[String] =
      new AtomicReference[String]("foo")

    val readFile: IO[String] =
      IO(fileContents.get())

    val appendFile: String => IO[Unit] =
      contents => IO(fileContents.getAndUpdate(_ + lineSeparator() + contents))

    val program: IO[(String, String)] =
      for {
        contents <- readFile
        _ <- appendFile("bar")
        updatedContents <- readFile
      } yield (contents, updatedContents)

    val (originalContents, updatedContents) =
      program.unsafeRunSync()

    assertEquals(originalContents, "foo")
    assertEquals(updatedContents, "foo" + lineSeparator() + "bar")
  }

  property("IO Applicative function") {
    import tech.backwards.fp.learn.Applicative.syntax._
    import tech.backwards.fp.learn.Functor.syntax.function._

    def add(x: Int, y: Int, z: Int): Int =
      x + y + z

    val program: IO[Int] =
      (add _).curried `<$>` IO(1) <*> IO(2) <*> IO(3)

    assertEquals(program.unsafeRunSync(), 6)
  }
}