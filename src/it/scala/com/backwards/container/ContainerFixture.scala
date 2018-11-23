package com.backwards.container

import org.scalatest.Suite

trait ContainerFixture {
  this: Suite with ContainerLifecycle =>

  def container: Container
}