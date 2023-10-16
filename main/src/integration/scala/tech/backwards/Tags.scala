package tech.backwards

import munit.Tag

/**
 * https://www.jan0sch.de/post/scala-sbt-switch-from-integrationtest-config-to-tagged-tests/
 *
 * testOnly -- --include-tags=IntegrationTest
 *
 * testOnly -- --exclude-tags=IntegrationTest
 */
trait Tags {
  val tag: String => Tag =
    new Tag(_)

  val IntegrationTest: Tag =
    tag("IntegrationTest")
}
