package tech.backwards.fp.learn

import java.util.concurrent.atomic.AtomicInteger
import munit.ScalaCheckSuite
import org.scalacheck.Test

class IOSuite extends ScalaCheckSuite {
  override protected def scalaCheckTestParameters: Test.Parameters =
    super.scalaCheckTestParameters.withMinSuccessfulTests(100).withMaxDiscardRatio(10)

  property("Run IO") {
    val io: IO[String] =
      IO("foo")

    assertEquals(
      io.unsafeRunSync(),
      "foo"
    )
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

    val mappedIO =
      Functor[IO].fmap(io)(_ + "bar")

    assertEquals(
      mappedIO.unsafeRunSync(),
      "foobar"
    )
  }

  /*property("IO Functor syntax") {
    val io: IO[String] =
      IO(() => "foo")

    val mappedIO =
      Functor[IO].fmap(io)(_ + "bar")

    assertEquals(
      mappedIO.unsafeRunSync(),
      "foobar"
    )
  }*/

  // TODO - Monad

  // TODO - Applicative
}