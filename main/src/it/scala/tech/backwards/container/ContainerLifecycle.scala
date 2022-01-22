package tech.backwards.container

import org.scalatest._

sealed trait ContainerLifecycle extends SuiteMixin {
  this: Suite with ContainerFixture =>
}

trait ForAllContainerLifecycle extends ContainerLifecycle {
  this: Suite with ContainerFixture =>

  abstract override def run(testName: Option[String], args: Args): Status = {
    if (expectedTestCount(args.filter) == 0) {
      new CompositeStatus(Set.empty)
    } else try {
      scribe info s"Starting (for all) container ${container.imageName}"
      container.start()
      super.run(testName, args)
    } finally {
      scribe info s"Stopping (for all) container ${container.imageName}"
      container.stop()
    }
  }
}

trait ForEachContainerLifecycle extends ContainerLifecycle {
  this: Suite with OneInstancePerTest with ContainerFixture =>

  abstract protected override def runTest(testName: String, args: Args): Status =
    try {
      scribe info s"Starting (for each) container ${container.imageName}"
      container.start()
      super.runTest(testName, args)
    } finally {
      scribe info s"Stopping (for each) container ${container.imageName}"
      container.stop()
    }
}