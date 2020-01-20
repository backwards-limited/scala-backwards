package com.backwards.docker

import java.nio.file.{Files, Path, Paths, StandardOpenOption}
import java.util.UUID
import java.util.zip.{ZipEntry, ZipOutputStream}
import scala.collection.mutable.ListBuffer
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent._
import scala.concurrent.duration._
import scala.jdk.CollectionConverters._
import scala.language.postfixOps
import scala.sys.process.{Process, ProcessLogger}
import scala.util.{Failure, Success, Try}
import better.files._
import com.typesafe.scalalogging.LazyLogging

/**
  * TODO - WIP
  * @param name String
  * @param dockerComposeFiles Seq[File]
  * @param environment Map[String, String] Environment variables. Example: Map("CONTAINER_EXPOSED_PORT" -> "80")
  */
case class DockerCompose(
  name: String = UUID.randomUUID().toString,
  dockerComposeFiles: Seq[File],
  environment: Map[String, String] = Map.empty[String, String]
) extends LazyLogging {

  import DockerCompose._

  logger.info(s"Docker compose files: ${dockerComposeFiles.map(_.pathAsString).mkString(", ")}")

  private val defaultLongCommandTimeOut = 5 minutes
  private val defaultShortCommandTimeOut = 1 minute

  /**
    * Executes docker-compose up command using setup defined and waits for all containers with healthchecks to be in a healthy state.
    * @param timeOut maximum time for all containers be in a healthy state
    * @return true if all containers started and achieved a healthy state otherwise false
    */
  def up(timeOut: Duration): Boolean =
    dockerComposeUp && waitForAllHealthyContainers(timeOut)

  /**
    * Executes docker-compose down command using setup defined.
    * @return true if all containers were stopped and removed otherwise false
    */
  def down: Boolean =
    dockerComposeDown && checkContainersRemoval

  /**
    * Retrieves port that was mapped to binded port for specified service.
    * @param serviceName ServiceName
    * @param bindedPort port that was binded to some specific port
    * @return Returns a list of ports from all service containers that have binded port
    */
  def serviceMappedPort(serviceName: ServiceName, bindedPort: Int): Seq[String] = {
    serviceContainerIds(serviceName)
      .filter(containerMappedPort(_, bindedPort).nonEmpty)
      .map(containerMappedPort(_, bindedPort))
  }

  /**
    * Dump logs from STDOUT using docker-compose logs for each service.
    * Logs will be saved to target dir in a zip file with the name specified.
    * @param fileName zip file name
    * @param target directory where zip file will be saved
    * @return Try object that if fails contains exception, otherwise returns nothing
    */
  def dumpLogs(fileName: String, target: Path): Try[Unit] = {
    val files = services.flatMap { service =>
      createFile(Paths.get(s"log_$service"), containerLogs(Some(service))).toOption
    }

    zip(target.resolve(s"$fileName.zip").toAbsolutePath, files)
  }

  /**
    * Compresses files to zip file and deletes files.
    * @param filePath file path including file name and directory where it will be created
    * @param files list of files path
    */
  def zip(filePath: Path, files: Seq[Path]): Try[Unit] = Try {
    val zip = new ZipOutputStream(Files.newOutputStream(filePath, StandardOpenOption.CREATE))

    files.foreach { file =>
      zip.putNextEntry(new ZipEntry(file.toString))
      Files.copy(file, zip)
      Files.delete(file)
      zip.closeEntry()
    }

    zip.close()
  }

  /**
    * Creates a file, in filePath with the given content.
    * @param filePath path with file name
    * @param content list of strings that will be written in file
    * @return a Try object
    */
  def createFile(filePath: Path, content: List[String]): Try[Path] =
    Try(Files.write(filePath, content.asJava, StandardOpenOption.CREATE))

  /**
    * Removes network and containers started by setup if still present.
    * @return true if setup network and containers were removed with success otherwise false
    */
  def cleanUp: Boolean = {
    val containerIds = projectContainerIds

    val stoppedContainers = stopAllContainers(containerIds)
    val removedContainers = removeAllContainers(containerIds)
    val removedNetworks   = networkId(name).forall(removeNetwork)

    if (!stoppedContainers) {
      logger.error("Failed to stop containers...")
    }

    if (!removedContainers) {
      logger.error("Failed to remove containers...")
    }

    if (!removedNetworks) {
      logger.error("Failed to remove networks...")
    }

    stoppedContainers && removedContainers && removedNetworks
  }

  def containerMappedPort(serviceName: ServiceName, port: Int): Int =
    containerMappedPort(serviceContainerId(serviceName), port).toInt // TODO

  def containerMappedPort(containerId: String, port: Int): String = {
    val command = Seq(
      "docker",
      "port",
      containerId,
      port.toString
    )

    runCmdWithOutput(command, environment, defaultShortCommandTimeOut) match {
      case Success(output) if output.nonEmpty => output.head.replaceAll("^.+:", "")
      case _ => ""
    }
  }

  def serviceContainerId(serviceName: ServiceName): String =
    serviceContainerIds(serviceName).head // TODO - headOption ???

  def serviceContainerIds(serviceName: ServiceName): List[String] = {
    val command =
      Seq("docker-compose") ++
        composeFileArguments(dockerComposeFiles) ++
        Seq("-p", name, "ps", "-q", serviceName.value)

    runCmdWithOutput(command, environment, defaultShortCommandTimeOut) match {
      case Success(output) =>
        output
      case Failure(f) =>
        logger.error(s"Failed to get service container Ids!", f)
        List.empty
    }
  }

  def projectContainerIds: List[String] = {
    val command =
      Seq("docker-compose") ++
        composeFileArguments(dockerComposeFiles) ++
        Seq("-p", name, "ps", "-q")

    runCmdWithOutput(command, environment, defaultShortCommandTimeOut) match {
      case Success(output) =>
        output
      case Failure(f) =>
        logger.error(s"Failed to get all project container Ids!", f)
        List.empty
    }
  }

  def services: List[String] = {
    val command =
      Seq("docker-compose") ++
        composeFileArguments(dockerComposeFiles) ++
        Seq("-p", name, "config", "--services")

    runCmdWithOutput(command, environment, defaultShortCommandTimeOut) match {
      case Success(output) =>
        output
      case Failure(f) =>
        logger.error(s"Failed to get all services!", f)
        List.empty
    }
  }

  def isContainerWithHealthCheck(containerId: String): Boolean = {
    val command = Seq(
      "docker",
      "inspect",
      containerId,
      "--format={{ .State.Health }}"
    )

    runCmdWithOutput(command, environment, defaultShortCommandTimeOut) match {
      case Success(output) =>
        output.nonEmpty && output.size == 1 && output.head != "<nil>"
      case Failure(f) =>
        logger.error(s"Failed while checking if container $containerId contains healthcheck!", f)
        false
    }
  }

  def dockerComposeUp: Boolean = {
    val command =
      Seq("docker-compose") ++
        composeFileArguments(dockerComposeFiles) ++
        Seq("-p", name, "up", "-d")

    runCmd(command, environment, defaultLongCommandTimeOut)
  }

  def dockerComposeDown: Boolean = {
    val command =
      Seq("docker-compose") ++
        composeFileArguments(dockerComposeFiles) ++
        Seq("-p", name, "down")

    runCmd(command, environment, defaultLongCommandTimeOut)
  }

  def waitForHealthyContainer(containerId: String, timeout: Duration): Boolean = {
    val command = Seq(
      "docker",
      "inspect",
      containerId,
      "--format={{ .State.Health.Status }}"
    )

    val future = Future[Boolean] {
      var result = ""
      var done = false

      while (!done) {
        val eventBuilder = new StringBuilder()
        val process = Process(command).run(ProcessLogger(line => eventBuilder.append(line), line => logger.debug(line)))
        val exitValue = waitProcessExit(process, defaultShortCommandTimeOut)

        done = exitValue == 0 && eventBuilder.nonEmpty && eventBuilder.mkString == "healthy"

        if (done) {
          result = eventBuilder.mkString
        } else {
          Thread.sleep(1.second.toMillis)
        }
      }

      result.nonEmpty
    }

    try {
      Await.result(future, timeout)
    } catch {
      case e: TimeoutException =>
        logger.error(s"Container $containerId didn't change to healthy state in ${timeout.toSeconds} seconds", e)
        false
    }
  }

  def waitForAllHealthyContainers(timeout: Duration): Boolean =
    projectContainerIds
      .filter(isContainerWithHealthCheck)
      .forall(waitForHealthyContainer(_, timeout))

  def containerLogs(serviceName: Option[String]): List[String] = {
    val command =
      Seq("docker-compose") ++
        composeFileArguments(dockerComposeFiles) ++
        Seq("-p", name, "logs", "--no-color") ++
        serviceName

    runCmdWithOutput(command, environment, defaultShortCommandTimeOut) match {
      case Success(output) =>
        output
      case Failure(f) =>
        serviceName.foreach(name => logger.error(s"Failed while retrieving logs of $name", f))
        List.empty
    }
  }

  def checkContainersRemoval: Boolean = {
    val command =
      Seq("docker-compose") ++
        composeFileArguments(dockerComposeFiles) ++
        Seq("-p", name, "ps", "-q")

    runCmdWithOutput(command, environment, defaultShortCommandTimeOut) match {
      case Success(output) =>
        output.isEmpty
      case Failure(f) =>
        logger.error("Failed while checking containers removal", f)
        false
    }
  }

  private def stopAllContainers(ids: Seq[String]): Boolean =
    ids.forall { id =>
      val command = Seq("docker", "stop", id)
      runCmd(command, Map.empty, 1 minute)
    }

  private def removeAllContainers(ids: Seq[String]): Boolean =
    ids.forall { id =>
      val command = Seq("docker", "rm", "-f", id)
      runCmd(command, Map.empty, 1 minute)
    }

  private def networkId(network: String): Option[String] = {
    val command = Seq("docker", "network", "ls", "--filter", s"name=${network}_default", "-q")

    runCmdWithOutput(command, Map.empty, 10 seconds) match {
      case Success(list) => if (list.nonEmpty) list.headOption else None
      case Failure(_)    => None
    }
  }

  private def removeNetwork(networkId: String): Boolean = {
    val command = Seq("docker", "network", "rm", networkId)
    runCmd(command, Map.empty, 10 seconds)
  }

  private def waitProcessExit(process: Process, timeout: Duration): Int = {
    val future = Future(blocking(process.exitValue()))

    try {
      Await.result(future, timeout)
    } catch {
      case e: TimeoutException =>
        logger.error(s"Process didn't finish in ${timeout.toSeconds} seconds", e)
        process.destroy()
        process.exitValue()
    }
  }

  /**
    * Runs a command using specified working directory.
    * @param command list of strings that compose the command
    * @return a Try object with output lines list if succeeds
    */
  private def runCmdWithOutput(
    command: Seq[String],
    envVars: Map[String, String],
    timeout: Duration
  ): Try[List[String]] = Try {
    val resultBuffer = new ListBuffer[String]()

    val process =
      Process(command, None, envVars.toSeq: _*)
        .run(ProcessLogger(line => resultBuffer += line, line => logger.debug(line)))

    assert(waitProcessExit(process, timeout) == 0, s"Failed to run command: ${command.mkString(" ")}")
    resultBuffer.toList
  }

  /**
    * Runs a command using specified working directory.
    * @param command list of strings that compose the command
    * @return true if command ran with success
    */
  private def runCmd(command: Seq[String], envVars: Map[String, String], timeout: Duration): Boolean = {
    val process =
      Process(command, None, envVars.toSeq: _*).run(ProcessLogger(_ => (), line => logger.debug(line)))

    waitProcessExit(process, timeout) == 0
  }

  private def composeFileArguments(files: Seq[File]): Seq[String] =
    files.flatMap(file => Seq("-f", file.pathAsString))
}

object DockerCompose {
  case class ServiceName(value: String) extends AnyVal
}