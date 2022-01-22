package tech.backwards.fp

import java.nio.charset.{CharacterCodingException, StandardCharsets}
import cats.effect.kernel.Temporal
import cats.effect.unsafe.implicits.global
import org.scalatest.matchers.must.Matchers
import org.scalatest.wordspec.AnyWordSpec

/**
 * [[https://alexn.org/blog/2020/08/03/on-error-retry-loop.html Retry Failing Tasks]]
 */
class RetrySpec extends AnyWordSpec with Matchers {
  "Retry example without using a library such as cats-retry" should {
    import java.io._
    import cats.effect.IO

    // Nasty but just for demo purposes
    def readTextFromFile(file: File, charset: String): IO[String] =
      IO {
        val in = new BufferedReader(
          new InputStreamReader(
            new FileInputStream(file), charset
          )
        )

        val builder = new StringBuilder()
        var line: String = null

        do {
          line = in.readLine()

          if (line != null)
            builder.append(line).append("\n")
        } while (line != null)

        builder.toString
      }

    // The above is doing I/O - the file may be missing temporarily, so we could retry

    "show a naive implementation" in {
      /*
      ApplicativeError type class has:

      trait ApplicativeError[F[_], E] extends Applicative[F] {
        // Works like a flatMap operation, but for errors (the equivalent of Java/Scala’s catch statement)
        def handleErrorWith[A](fa: F[A])(f: E => F[A]): F[A]

        // Lifts an E error into the F[A] context (the equivalent of Java’s and Scala’s throw for exceptions)
        def raiseError[A](e: E): F[A]
      }
      */

      import cats.implicits._
      import cats.{ApplicativeError, Defer}

      object OnErrorRetry {
        // WARN: not OK, because we don't have an end condition!
        def adInfinitum[F[_]: ApplicativeError[*[_], Throwable]: Defer, A](fa: F[A]): F[A] =
          fa handleErrorWith { _ =>
            // Recursive call describing infinite loop
            Defer[F].defer(adInfinitum(fa))
          }
      }

      val content: IO[String] =
        OnErrorRetry.adInfinitum(readTextFromFile(new File("README.md"), StandardCharsets.UTF_8.name))

      // Fine as no retry will occur because the file exists
      content.unsafeRunSync() must startWith("# Main")

      // DO NOT RUN - This will loop for ever
      // OnErrorRetry.adInfinitum(readTextFromFile(new File("non-existing"), StandardCharsets.UTF_8.name)).unsafeRunSync()

      /*
      There’s a caveat with the way we’re using handleErrorWith in such recursive loops. The type we use might not have a memory-safe implementation.
      We use the Defer type class to force usage of memory-safe (trampolined) implementations, although its laws are probably not strong enough.
      Still, this restriction will do just fine in practice.
      */

      /*
      This sample has several problems that we’ll have to address:
      - no end condition
      - no filtering of errors, since not all errors are recoverable
      - no protections, like “exponential backoff”
      */
    }

    "show filtering and end condition" in {
      /*
      We must only retry the task in situations in which it can be retried.
      And there has to be an end condition in that loop.
      */

      import cats.implicits._
      import cats.{ApplicativeError, Defer}

      /** Signaling desired outcomes via Boolean is very confusing, having our own ADT for this is better. */
      sealed trait RetryOutcome

      object RetryOutcome {
        final case object Next extends RetryOutcome

        final case object Raise extends RetryOutcome
      }

      /** Module grouping our retry helpers. */
      object OnErrorRetry {
        def withAtMost[F[_]: ApplicativeError[*[_], Throwable]: Defer, A](fa: F[A], maxRetries: Int)(p: Throwable => RetryOutcome): F[A] =
          fa handleErrorWith { error =>
            if (maxRetries > 0)
              p(error) match {
                case RetryOutcome.Next =>
                  Defer[F].defer(withAtMost(fa, maxRetries - 1)(p)) // Recursive call

                case RetryOutcome.Raise =>
                  ApplicativeError[F, Throwable].raiseError(error) // Cannot recover from error
              }
            else
            // Maximum retries reached, triggering error
              ApplicativeError[F, Throwable].raiseError(error)
          }
      }

      def contents(file: File): IO[String] =
        OnErrorRetry.withAtMost(readTextFromFile(file, StandardCharsets.UTF_8.name), maxRetries = 10) {
          case _: CharacterCodingException =>
            RetryOutcome.Raise
          case _ =>
            RetryOutcome.Next
        }

      contents(new File("README.md")).unsafeRunSync() must startWith("# Main")

      val Left(exception) = contents(new File("non-existing")).attempt.unsafeRunSync()
      exception mustBe a [FileNotFoundException]
      exception.getMessage mustBe "non-existing (No such file or directory)"
    }

    "show generic retry" in {
      import cats.implicits._
      import cats.{ApplicativeError, Defer}

      /** Signaling desired outcomes via Boolean is very confusing, having our own ADT for this is better. */
      sealed trait RetryOutcome

      object RetryOutcome {
        final case object Next extends RetryOutcome

        final case object Raise extends RetryOutcome
      }

      object OnErrorRetry {
        /** Saves us from describing recursive functions that accumulate state. */
        def loop[F[_]: ApplicativeError[*[_], Throwable]: Defer, A, S](fa: F[A], state: S)(f: (Throwable, S, S => F[A]) => F[A]): F[A] =
          fa handleErrorWith { err =>
            f(err, state, state => Defer[F].defer(loop(fa, state)(f)))
          }

        def withAtMost[F[_]: ApplicativeError[*[_], Throwable]: Defer, A](fa: F[A], maxRetries: Int)(p: Throwable => RetryOutcome): F[A] =
          loop(fa, maxRetries) { (error, retriesLeft, retry) =>
            if (retriesLeft > 0)
              p(error) match {
                case RetryOutcome.Next =>
                  retry(retriesLeft - 1)
                case RetryOutcome.Raise =>
                  // Cannot recover from error
                  ApplicativeError[F, Throwable].raiseError(error)
              }
            else
              // Maximum retries reached, triggering error
              ApplicativeError[F, Throwable].raiseError(error)
          }
      }

      // Retrying 10 times at most
      def contents(file: File): IO[String] = OnErrorRetry.withAtMost(readTextFromFile(file, StandardCharsets.UTF_8.name), maxRetries = 10) {
        case _: CharacterCodingException =>
          RetryOutcome.Raise
        case _ =>
          RetryOutcome.Next
      }

      contents(new File("README.md")).unsafeRunSync() must startWith("# Main")

      val Left(exception) = contents(new File("non-existing")).attempt.unsafeRunSync()
      exception mustBe a [FileNotFoundException]
      exception.getMessage mustBe "non-existing (No such file or directory)"
    }

    "show exponential backoff" in {
      import scala.concurrent.duration._
      import cats.implicits._
      import cats.{ApplicativeError, MonadError, Defer}

      /** Signaling desired outcomes via Boolean is very confusing, having our own ADT for this is better. */
      sealed trait RetryOutcome

      object RetryOutcome {
        final case object Next extends RetryOutcome

        final case object Raise extends RetryOutcome
      }

      /**
       * Configuration for retry logic, could be read from a config file, via something like [[https://github.com/pureconfig/pureconfig PureConfig]].
       */
      final case class RetryConfig(
        maxRetries: Int,
        initialDelay: FiniteDuration,
        maxDelay: FiniteDuration,
        backoffFactor: Double,
        private val evolvedDelay: Option[FiniteDuration] = None,
      ) {
        def canRetry: Boolean = maxRetries > 0

        def delay: FiniteDuration =
          evolvedDelay.getOrElse(initialDelay)

        def evolve: RetryConfig =
          copy(
            maxRetries = math.max(maxRetries - 1, 0),
            evolvedDelay = Some {
              val nextDelay = evolvedDelay.getOrElse(initialDelay) * backoffFactor

              maxDelay.min(nextDelay) match {
                case ref: FiniteDuration => ref
                case _: Duration.Infinite => maxDelay
              }
            }
          )
      }

      object OnErrorRetry {
        /** Saves us from describing recursive functions that accumulate state. */
        def loop[F[_]: ApplicativeError[*[_], Throwable]: Defer, A, S](fa: F[A], state: S)(f: (Throwable, S, S => F[A]) => F[A]): F[A] =
          fa handleErrorWith { err =>
            f(err, state, state => Defer[F].defer(loop(fa, state)(f)))
          }

        def withBackoff[F[_]: MonadError[*[_], Throwable]: Defer: Temporal, A](fa: F[A], config: RetryConfig)(p: Throwable => F[RetryOutcome]): F[A] =
          loop(fa, config) { (error, state, retry) =>
            if (state.canRetry)
              p(error).flatMap {
                case RetryOutcome.Next =>
                  Temporal[F].sleep(state.delay) *> retry(state.evolve)
                case RetryOutcome.Raise =>
                  // Cannot recover from error
                  MonadError[F, Throwable].raiseError(error)
              }
            else
              // No retries left
              MonadError[F, Throwable].raiseError(error)
          }
      }

      import cats.effect.{ExitCode, IOApp}
      import com.typesafe.scalalogging.LazyLogging

      object App extends LazyLogging with IOApp {
        override def run(args: List[String]): IO[ExitCode] = {
          val config = RetryConfig(
            maxRetries = 10,
            initialDelay = 10.millis,
            maxDelay = 2.seconds,
            backoffFactor = 1.5
          )

          val task = IO.defer {
            val path = args.headOption.getOrElse(
              throw new IllegalArgumentException("File path expected in main's args")
            )

            readTextFromFile(new File(path), "UTF-8")
          }

          val taskWithRetries = OnErrorRetry.withBackoff(task, config) {
            case _: CharacterCodingException | _: IllegalArgumentException =>
              IO.pure(RetryOutcome.Raise)

            case e =>
              IO(logger.warn("Unexpected error, retrying", e)).as(RetryOutcome.Next)
          }

          for {
            t <- taskWithRetries
            _ <- IO(println(t))
          } yield ExitCode.Success
        }
      }
    }
  }
}