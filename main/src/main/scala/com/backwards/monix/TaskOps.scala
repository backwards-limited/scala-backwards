package com.backwards.monix

import scala.language.implicitConversions
import monix.eval.Task

object TaskOps extends TaskOps

trait TaskOps {
  implicit def task2Ops(task: Task.type): Ops.type = Ops

  object Ops {
    def seq[T](ts: Task[T]*): Task[Seq[T]] = Task sequence ts
  }
}