package tech.backwards.io

import java.util.concurrent.atomic.AtomicBoolean
import scala.concurrent.{ExecutionContext, Future}
import scala.io.StdIn

trait Continue {
  val continue = new AtomicBoolean(true)

  def checkContinue(implicit ec: ExecutionContext): Future[Unit] = {
    println("\nAny key and <enter> to QUIT\n")

    Future {
      StdIn.readLine()
      continue set false
    }
  }
}