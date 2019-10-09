package com.backwards.transform

import scala.concurrent.duration._
import scala.concurrent.{Await, Future}
import scala.language.{higherKinds, postfixOps}
import cats.effect.{ContextShift, IO}
import cats.{Id, ~>}

/**
  * See [[cats.arrow.FunctionK]]
  */
trait Transform {
  implicit val contextShift: ContextShift[IO] =
    cats.effect.IO.contextShift(scala.concurrent.ExecutionContext.Implicits.global)

  implicit class LiftTo[X[_], A](x: X[A]) {
    def liftTo[Y[_]](implicit transform: X ~> Y): Y[A] = transform(x)
  }

  implicit val `Future ~> Id`: Future ~> Id = new (Future ~> Id) {
    def apply[A](future: Future[A]): Id[A] =
      Await.result(future, 30 seconds) // TODO - Hardcoded duration ??
  }

  implicit val `Future ~> IO`: Future ~> IO = new (Future ~> IO) {
    override def apply[A](future: Future[A]): IO[A] =
      IO fromFuture IO(future)
  }
}

object Transform extends Transform