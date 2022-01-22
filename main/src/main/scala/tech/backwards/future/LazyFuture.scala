package tech.backwards.future

import scala.concurrent.{ExecutionContext, Future}
import cats.Monad
import cats.implicits._

trait LazyFuture[A] extends (ExecutionContext => Future[A]) {
  def run(ec: ExecutionContext = ExecutionContext.Implicits.global): Future[A] =
    apply(ec)
}

object LazyFuture {
  implicit def lazyFutureMonad: Monad[LazyFuture] = new Monad[LazyFuture] {
    def flatMap[A, B](fa: LazyFuture[A])(f: A => LazyFuture[B]): LazyFuture[B] = { implicit ec: ExecutionContext =>
      fa.run(ec).flatMap(a => f(a).run(ec))
    }

    // TODO Laws testing doesn't like this
    def tailRecM[A, B](a: A)(f: A => LazyFuture[A Either B]): LazyFuture[B] = { implicit ec: ExecutionContext =>
      // FlatMap[Future].tailRecM(a)(a => f(a).run(ec))

      f(a).run(ec).flatMap {
        case a @ Left(_) => tailRecM(a.value)(f).run(ec)
        case Right(b) => Future(b)
      }
    }

    def pure[A](x: A): LazyFuture[A] = { implicit ec: ExecutionContext =>
      Future(x)
    }
  }
}