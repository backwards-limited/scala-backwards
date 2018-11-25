package com.backwards.future

import scala.concurrent.{ExecutionContext, Future}
import cats.Monad

trait LazyFuture[A] extends (ExecutionContext => Future[A]) {
  def run(ec: ExecutionContext = ExecutionContext.Implicits.global): Future[A] = apply(ec)
}

object LazyFuture {
  implicit def lazyFutureMonad: Monad[LazyFuture] = new Monad[LazyFuture] {
    def flatMap[A, B](fa: LazyFuture[A])(f: A => LazyFuture[B]): LazyFuture[B] = ???

    def tailRecM[A, B](a: A)(f: A => LazyFuture[A Either B]): LazyFuture[B] = ???

    def pure[A](x: A): LazyFuture[A] = ???
  }
}