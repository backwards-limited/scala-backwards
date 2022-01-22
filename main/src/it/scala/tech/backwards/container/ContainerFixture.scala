package tech.backwards.container

import org.scalatest.Suite

@deprecated(message = "Scala version of TestContainers now does the job, so use it", since = "9th February 2020")
trait ContainerFixture {
  this: Suite with ContainerLifecycle =>

  def container: Container
}