package tech.backwards.maths

import scala.io.StdIn.readLine
import cats.data.OptionT
import cats.effect.{ExitCode, IO, IOApp}
import cats.implicits._
import tech.backwards.maths.Eval._

object EvalApp extends IOApp {
  val readLn: IO[String] =
    IO(readLine())

  val putStrLn: String => IO[Unit] =
    s => IO(println(s))

  def run(args: List[String]): IO[ExitCode] = {
    lazy val program: OptionT[IO, Unit] = for {
      _ <- OptionT liftF putStrLn("Provide a simple mathematical expression (or q/Q to quit)? E.g. (2.16 - 48.34) ^ -1")
      s <- OptionT(readLn.map(s => if (s.toLowerCase == "q") none else s.some))
      _ <- OptionT liftF eval(s).fold(putStrLn, result => putStrLn(s"Result = $result"))
      _ <- program
    } yield ()

    program.value.map(_ => ExitCode.Success)
  }
}
