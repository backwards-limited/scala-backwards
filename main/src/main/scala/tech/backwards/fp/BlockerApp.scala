package tech.backwards.fp

import java.nio.file.{Files, Path, Paths}
import scala.io.StdIn
import cats.effect._
import cats.implicits.catsSyntaxApplicativeByName

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
  // The following was Cats Effect 2 - Upgraded to Cats Effect 3
  /*def app: IO[Unit] = Blocker[IO]
    .use { blocker =>
      for {
        _    <- printLine("Enter path:")
        line <- blocker.blockOn(readLineBlocking)
        path <- IO(Paths.get(line))
        _    <- safeCreate(path)(blocker)
      } yield ()
    }*/
  
  def app: IO[Unit] =
    for {
      _ <- IO(printLine("Enter path:"))
      line <- readLineBlocking
      path <- IO(Paths.get(line))
      _ <- safeCreate(path)
    } yield ()

  def safeCreate(path: Path): IO[Unit] =
    for {
      // We can block on io
      alreadyExists <- checkIfExists(path)
      // Or create effect running on blocking EC
      _ <- IO.blocking(Files.createFile(path)).unlessA(alreadyExists)
    } yield ()

  def checkIfExists(path: Path): IO[Boolean] =
    IO.blocking(Files.exists(path))
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
  // Following was Cats Effect 2 - Upgraded to Cats Effect 3
  /*val customBlockingEC: ExecutionContextExecutorService =
    ExecutionContext fromExecutorService Executors.newCachedThreadPool { (r: Runnable) =>
      val t = new Thread(r)
      t setName s"custom-blocking-ec-${t.getName}"
      t
    }*/

  def displayNameWithBlocker: IO[Unit] =
    for {
      _    <- printLine("Enter your name:")
      name <- readLineBlocking
      _    <- printLine(s"Hello, $name")
    } yield ()

  override def run(args: List[String]): IO[ExitCode] =
    displayNameWithBlocker
      .as(ExitCode.Success)
      // .guarantee(IO(customBlockingEC.shutdownNow())) Cats Effect 2
}

trait Actions {
  def printLine(s: String): IO[Unit] =
    for {
      _ <- IO(println(s))
      _ <- showThread
    } yield ()

  def readLineBlocking: IO[String] = for {
    line <- IO.blocking(StdIn.readLine())
    _ <- showThread
  } yield line

  def showThread: IO[Unit] =
    IO delay println(
      Console.RED + s"<<Operation executed on ${Thread.currentThread().getName}>>" + Console.RESET
    )
}