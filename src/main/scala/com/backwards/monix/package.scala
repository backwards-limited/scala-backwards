package com.backwards

import scala.language.implicitConversions
import _root_.monix.eval.Task

package object monix {
  object TaskOps {
    def seq[T](ts: Task[T]*): Task[Seq[T]] = Task sequence ts
  }

  implicit def task2Ops(task: Task.type): TaskOps.type = TaskOps
}