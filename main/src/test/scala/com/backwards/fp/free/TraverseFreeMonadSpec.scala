package com.backwards.fp.free

import scala.concurrent.Future
import cats.{Foldable, Monad, Monoid}
import cats.implicits._
import org.scalatest.matchers.must.Matchers
import org.scalatest.wordspec.AnyWordSpec
import scala.concurrent.ExecutionContext.Implicits.global

class TraverseFreeMonadSpec extends AnyWordSpec with Matchers {
  "Travers sequentially" should {
    "be performed naively" in {
      /*def blah[F[_]: Foldable: Monoid, A, M[_]: Monad, B](f: F[A])(fn: A => M[B]) = {

        f.foldLeft(Monad[M].pure(Monoid[F].empty)) {
          case (acc, a) =>
            acc.flatMap(f => fn(a).map(b => f .combine(b.pure[F])))
            ???
        }
        ???
      }*/

      def sequentialTraverseHardCodedEffectsd[A, B](seq: Seq[A])(f: A => Future[B]): Future[Seq[B]] =
        seq.foldLeft(Future.successful(Seq.empty[B])) {
          case (acc, nxt) => {
            acc.flatMap(bs => f(nxt).map(b => bs :+ b))
          }
        }

      def blah[F[_], A, B](fa: F[A])(fn: A => Future[B])(implicit FoldableF: Foldable[F], MonoidB: Monoid[B], MonadF: Monad[F]): Future[F[B]] = {
        FoldableF.foldLeft(fa, Future.successful(MonadF.pure(MonoidB.empty))) {
          case (acc, a) =>
            acc.flatMap(futureB => fn(a).map(b => MonadF.map(futureB)(bb => MonoidB.combine(bb, b))))
        }
      }


      def blah1[F[_]: Foldable, A, B](fa: F[A])(fn: A => Future[B])(implicit MonoidB: Monoid[B], MonadF: Monad[F]): Future[F[B]] = {
        fa.foldLeft(Future.successful(MonadF.pure(MonoidB.empty))) {
          case (acc, a) =>
            acc.flatMap(futureB => fn(a).map(b => MonadF.map(futureB)(bb => MonoidB.combine(bb, b))))
        }
      }

      def blah2[F[_]: Foldable, A, B: Monoid](fa: F[A])(fn: A => Future[B])(implicit MonadF: Monad[F]): Future[F[B]] = {
        fa.foldLeft(Future.successful(MonadF.pure(Monoid[B].empty))) {
          case (acc, a) =>
            acc.flatMap(futureB => fn(a).map(b => MonadF.map(futureB)(_ |+| b)))
        }
      }

      def blah3[F[_]: Foldable: Monad, A, B: Monoid](fa: F[A])(fn: A => Future[B]): Future[F[B]] = {
        fa.foldLeft(Future.successful(Monoid[B].empty.pure[F])) {
          case (acc, a) =>
            acc.flatMap(futureB => fn(a).map(b => futureB.map(_ |+| b)))
        }
      }

      def sequentialTraverse[F[_]: Foldable: Monad, A, G[_]: Monad, B: Monoid](
        fa: F[A]
      )(fn: A => G[B]): G[F[B]] =
        fa.foldLeft(Monoid[B].empty.pure[F].pure[G]) { case (acc, a) =>
          acc.flatMap(gb => fn(a).map(b => gb.map(_ |+| b)))
        }



      /*def sequentialTraverse[C[_], A[_], X, Y](c: C[X])(f: X => A[Y])(
        implicit fd: Foldable[C],
        mn: Monad[A],
        monoid: Monoid[C]
      ): A[C[Y]] = {
        fd.foldLeft(c, mn.pure(monoid.empty)) {
          case (acc, nxt) =>
            mn.flatMap(acc) { ys => f(nxt).map(y => monoid.append(ys, y)) }
        }
      }*/
    }
  }
}