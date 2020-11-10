package com.backwards.monix

import monix.eval.Task
import monix.execution.Scheduler.Implicits.global
import org.scalatest.matchers.must.Matchers
import org.scalatest.wordspec.AnyWordSpec
import com.backwards.monix.TaskOps._

class TaskOpsSpec extends AnyWordSpec with Matchers {
  "Tasks" should {
    "be run sequentially" in {
      lazy val task1 = "Task 1"
      lazy val task2 = "Task 2"

      val tasks: Task[Seq[String]] = Task.seq(
        Task(task1),
        Task(task2)
      )

      tasks.runSyncUnsafe() mustEqual Seq(task1, task2)
    }
  }
}