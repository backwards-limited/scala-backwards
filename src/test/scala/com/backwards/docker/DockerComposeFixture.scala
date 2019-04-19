package com.backwards.docker

import java.nio.file.Path
import java.util.concurrent.Semaphore
import java.util.concurrent.atomic.AtomicBoolean
import scala.concurrent.duration._
import scala.language.postfixOps
import scala.util.{Failure, Success}
import org.scalatest._
import com.typesafe.scalalogging.LazyLogging

// TODO - WIP
trait DockerComposeFixture extends SuiteMixin with LazyLogging {
  this: Suite =>

  val dockerCompose: DockerCompose

  def keepContainersOnSuccess: Boolean = false
  def keepContainersOnFailure: Boolean = false

  def logDumpLocation: Option[Path] = None
  def logDumpFileName: Option[String] = None

  def containerStartUpTimeout: Option[Duration] = None

  /**
    * Semaphore used to control the number of test running in parallel
    */
  def parallelTestLimitSemaphore: Semaphore = new Semaphore(1, true)

  private val dockerContainersUp: AtomicBoolean = new AtomicBoolean(false)

  abstract override def run(testName: Option[String], args: Args): Status =
    if (expectedTestCount(args.filter) == 0) {
      new CompositeStatus(Set.empty)
    } else {
      dockerComposeUp()

      try super.run(testName, args)
      finally dockerComposeDown()
    }

  private def dockerComposeUp(): Unit = {
    parallelTestLimitSemaphore.acquire()

    logger.info(s"Starting containers (${dockerCompose.name}) ...")
    val started = dockerCompose.up(containerStartUpTimeout getOrElse 5.minutes)
    dockerContainersUp.set(!started)
    assert(started, s"Failed to start containers in test ${dockerCompose.name}!")
    logger.info(s"Containers (${dockerCompose.name}) started!")
  }

  private def dockerComposeDown(): Unit =
    try {
      if (dockerContainersUp.get()) for {
        dumpLocation <- logDumpLocation
        dumpFileName <- logDumpFileName
      } dockerCompose.dumpLogs(dumpFileName, dumpLocation) match {
        case Success(_) => ()
        case Failure(f) => logger.error("Failed to dump logs!", f)
      }

      val keep = (keepContainersOnSuccess && !dockerContainersUp.get()) || (keepContainersOnFailure && dockerContainersUp.get())

      if (!keep) {
        logger.info(s"Removing containers (${dockerCompose.name}) ...")
        val removed = dockerCompose.down
        assert(removed, s"Failed to remove containers in test ${dockerCompose.name}!")
        logger.info(s"Containers (${dockerCompose.name}) removed!")
      }
    } finally {
      parallelTestLimitSemaphore.release()
    }
}