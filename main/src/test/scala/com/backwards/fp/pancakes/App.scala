package com.backwards.fp.pancakes

import cats.effect.ExitCode
import monix.eval.{Task, TaskApp}
import monix.execution.Scheduler.global
import sttp.tapir.server.http4s.Http4sServerInterpreter.toRoutes
import org.http4s.HttpRoutes
import org.http4s.implicits.http4sKleisliResponseSyntaxOptionT
import org.http4s.server.Router
import org.http4s.server.blaze.BlazeServerBuilder
import com.typesafe.scalalogging.StrictLogging
import com.backwards.fp.pancakes.Server.pancakesServerEndpoint

object App extends TaskApp with StrictLogging {
  // Interpreting the combined endpoint+logic as http4s's HttpRoutes
  val pancakesServerRoutes: HttpRoutes[Task] = toRoutes(pancakesServerEndpoint)

  def run(args: List[String]): Task[ExitCode] =
    BlazeServerBuilder[Task](global)
      .bindHttp(8080, "localhost")
      .withHttpApp(Router("/" -> pancakesServerRoutes).orNotFound)
      .resource
      .use(_ => Task(logger.info("Server started")) >> Task.never)
      .as(ExitCode.Success)
}