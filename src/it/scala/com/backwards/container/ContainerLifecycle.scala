package com.backwards.container

import org.scalatest._
import com.backwards.logging.Logging

sealed trait ContainerLifecycle extends SuiteMixin with Logging {
  this: Suite with ContainerFixture =>
}

trait ForAllContainerLifecycle extends ContainerLifecycle {
  this: Suite with ContainerFixture =>

  abstract override def run(testName: Option[String], args: Args): Status = {
    if (expectedTestCount(args.filter) == 0) {
      new CompositeStatus(Set.empty)
    } else try {
      info(s"Starting (for all) container ${container.imageName}")
      container.start()
      super.run(testName, args)
    } finally {
      info(s"Stopping (for all) container ${container.imageName}")
      container.stop()
    }
  }
}

trait ForEachContainerLifecycle extends ContainerLifecycle {
  this: Suite with OneInstancePerTest with ContainerFixture =>

  abstract protected override def runTest(testName: String, args: Args): Status =
    try {
      info(s"Starting (for each) container ${container.imageName}")
      container.start()
      super.runTest(testName, args)
    } finally {
      info(s"Stopping (for each) container ${container.imageName}")
      container.stop()
    }
}