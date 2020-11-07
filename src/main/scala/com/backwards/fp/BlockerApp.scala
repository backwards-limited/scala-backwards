package com.backwards.fp

import java.nio.file.{Files, Path, Paths}
import java.util.concurrent.{Executors, TimeUnit}
import scala.concurrent.{ExecutionContext, ExecutionContextExecutorService}
import scala.io.StdIn
import cats.effect.{Blocker, ContextShift, ExitCode, IO, IOApp, Resource, Sync}
import cats.implicits.catsSyntaxApplicative

/**
 * [[https://blog.softwaremill.com/a-short-guide-to-blocker-6cd29e145a12 A Short Guide to Blocker]]
 *
 * There are several methods to produce Blocker.
 * First of all, we can create it by calling the apply method, which will return Resource[F, Blocker].
 * New Blocker instance will be backed by a cached thread pool. All spawned threads will be daemon threads.
 */
object BlockerApp extends IOApp with Actions {
  override def run(args: List[String]): IO[ExitCode] =
    app.as(ExitCode.Success)

  // Blocker.apply returns resource which will automatically close underlying EC
  def app: IO[Unit] = Blocker[IO]
    .use { blocker =>
      for {
        _    <- printLine("Enter path:")
        line <- blocker.blockOn(readLineBlocking)
        path <- IO(Paths.get(line))
        _    <- safeCreate(path)(blocker)
      } yield ()
    }

  def safeCreate(path: Path)(blocker: Blocker): IO[Unit] =
    for {
      // We can block on io
      alreadyExists <- blocker.blockOn(checkIfExists(path))
      // Or create effect running on blocking EC
      _ <- blocker.delay[IO, Unit](Files.createFile(path)).unlessA(alreadyExists)
    } yield ()

  def checkIfExists(path: Path): IO[Boolean] =
    IO(Files.exists(path))
}

/**
 * If we want to use customized executor we can construct Blocker with Blocker.fromExecutorService.
 * It expects effect creating ExecutorService as an argument. That method also returns Resource.
 *
 * We can also create Blocker by lifting existing ExecutionConcext or ExecutorService with methods Blocker.liftExecutorService and Blocker.liftExecutionContext.
 *
 * These methods return a plain instance of Blocker, instead of Resource. So in this case, it’s our responsibility to shut down executors.
 */
object CustomBlockerApp extends IOApp with Actions {
  val customBlockingEC: ExecutionContextExecutorService =
    ExecutionContext fromExecutorService Executors.newCachedThreadPool { (r: Runnable) =>
      val t = new Thread(r)
      t setName s"custom-blocking-ec-${t.getName}"
      t
    }

  def displayNameWithBlocker(blocker: Blocker): IO[Unit] =
    for {
      _    <- printLine("Enter your name:")
      name <- blocker.blockOn(readLineBlocking)
      _    <- printLine(s"Hello, $name")
    } yield ()

  override def run(args: List[String]): IO[ExitCode] = {
    val blocker = Blocker.liftExecutionContext(customBlockingEC)

    displayNameWithBlocker(blocker)
      .as(ExitCode.Success)
      .guarantee(IO(customBlockingEC.shutdownNow()))
  }
}

object BeforeWeHadBlockerApp extends IOApp {
  val blockingExecutionContextResource: Resource[IO, ExecutionContext] =
    Resource.make(IO(ExecutionContext.fromExecutorService(Executors.newCachedThreadPool())))(ec => IO(ec.shutdown()))

  val jdbc: Resource[IO, JDBC[IO]] =
    blockingExecutionContextResource.map { blockingExecutionContext =>
      JDBC.mock(blockingExecutionContext)
    }

  def run(args: List[String]): IO[ExitCode] =
    jdbc.use(_.execute).as(ExitCode.Success)

  trait JDBC[F[_]] {
    def execute: F[Unit]
  }

  object JDBC {
    def mock[F[_]: ContextShift: Sync](blockingExecutionContext: ExecutionContext): JDBC[F] =
      new JDBC[F] {
        def execute: F[Unit] =
          ContextShift[F].evalOn(blockingExecutionContext)(Sync[F].delay {
            println("Executing JDBC call...")
            TimeUnit.SECONDS.sleep(2)
          })
      }
  }
}

object AfterWeHadBlockerApp extends IOApp {
  // Instead of ExecutionContext, we'll use Blocker
  val blockerResource: Resource[IO, Blocker] = Blocker[IO]

  val jdbc: Resource[IO, JDBC[IO]] =
    blockerResource.map { blocker =>
      JDBC.mock(blocker.blockingContext)
    }

  def run(args: List[String]): IO[ExitCode] =
    jdbc.use(_.execute).as(ExitCode.Success)

  trait JDBC[F[_]] {
    def execute: F[Unit]
  }

  object JDBC {
    def mock[F[_]: ContextShift: Sync](blockingExecutionContext: ExecutionContext): JDBC[F] =
      new JDBC[F] {
        def execute: F[Unit] =
          ContextShift[F].evalOn(blockingExecutionContext)(Sync[F].delay {
            println("Executing JDBC call...")
            TimeUnit.SECONDS.sleep(2)
          })
      }
  }
}

object AfterWeHadBlockerAlternativeApp extends IOApp {
  // Instead of ExecutionContext, we'll use Blocker
  val jdbc: Resource[IO, JDBC[IO]] =
    Blocker[IO].map { blocker =>
      JDBC.mock(blocker)
    }

  def run(args: List[String]): IO[ExitCode] =
    jdbc.use(_.execute).as(ExitCode.Success)

  trait JDBC[F[_]] {
    def execute: F[Unit]
  }

  object JDBC {
    def mock[F[_]: ContextShift: Sync](blocker: Blocker): JDBC[F] =
      new JDBC[F] {
        def execute: F[Unit] = {
          blocker.blockOn(Sync[F].delay {
            println("Executing JDBC call...")
            TimeUnit.SECONDS.sleep(2)
          })
        }
      }
  }
}

trait Actions {
  def printLine(s: String): IO[Unit] =
    for {
      _ <- IO(println(s))
      _ <- showThread
    } yield ()

  def readLineBlocking: IO[String] = for {
    line <- IO(StdIn.readLine())
    _ <- showThread
  } yield line

  def showThread: IO[Unit] =
    IO delay println(
      Console.RED + s"<<Operation executed on ${Thread.currentThread().getName}>>" + Console.RESET
    )
}