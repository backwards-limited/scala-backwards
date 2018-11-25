package com.backwards.future

import scala.concurrent.{ExecutionContext, Future}

trait LazyFuture[A] extends (ExecutionContext => Future[A]) {
  def run(ec: ExecutionContext = ExecutionContext.Implicits.global): Future[A] = apply(ec)
}