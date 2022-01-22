package tech.backwards.fp.effects

import scala.concurrent.ExecutionContext
import cats.effect.{ExitCode, IO, IOApp}
import cats.implicits.catsSyntaxFlatMapOps
import org.scalatest.matchers.must.Matchers
import org.scalatest.wordspec.AnyWordSpec

/**
 * [[https://timwspence.github.io/blog/posts/2021-01-12-threading-best-practices-cats-effect.html Threading best practices in Cats Effect]]
 */
class ContextShiftSpec extends AnyWordSpec with Matchers {
  "" should {
    "" in {

    }
  }
}

// Cats Effect 2 - Upgraded to Cats Effect 3
/*
object ContextShiftApp extends IOApp {
  def run(args: List[String]): IO[ExitCode] = {
    println(s"App: ${Thread.currentThread.getName}")

    contextShift.evalOn(ExecutionContext.global)(IO(println(s"I run on the blocking pool: ${Thread.currentThread.getName}"))) >>
      IO(println(s"I am shifted onto the pool that CS represents: ${Thread.currentThread.getName}")) >>
      contextShift.shift >>
      IO(println(s"I run on some pool: ${Thread.currentThread.getName}")) >>
      contextShift.shift >>
      IO(println(s"I run on the pool that CS represents: ${Thread.currentThread.getName}")).as(ExitCode.Success)
  }
}*/
