package com.backwards.docker

import java.nio.file.Paths
import java.time.Instant
import scala.language.postfixOps
import cats.implicits._
import org.scalatest.matchers.must.Matchers
import org.scalatest.wordspec.AnyWordSpec

class DockerComposeSpec extends AnyWordSpec with Matchers with DockerComposeFixture {
  val dockerCompose: DockerCompose =
    DockerCompose("kafka", Seq(Paths.get("src", "it", "resources", "docker-compose.yml")))

  val now: Instant = Instant.now

  "Docker compose" should {
    "start and stop containers" in {
      dockerCompose.services must contain only("zookeeper", "kafka")
    }
  }
}