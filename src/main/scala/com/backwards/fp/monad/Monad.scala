package com.backwards.fp.monad

import scala.language.{higherKinds, implicitConversions}
import com.backwards.fp.functor.Functor

abstract class Monad[M[_]: Functor] {
  def pure[A](a: A): M[A]

  def flatMap[A, B](m: M[A])(f: A => M[B]): M[B]
}

object Monad {
  def apply[M[_]: Monad]: Monad[M] = implicitly
}

class MonadPureOps[A](a: A) {
  def pure[M[_]: Monad]: M[A] = Monad[M] pure a
}

class MonadOps[M[_]: Monad, A](m: M[A]) {
  def flatMap[B](f: A => M[B]): M[B] = Monad[M].flatMap(m)(f)

  def >>=[B](f: A => M[B]): M[B] = flatMap(f)
}

object MonadOps {
  implicit def toMonadPureOps[A](a: A): MonadPureOps[A] = new MonadPureOps(a)

  implicit def toMonadOps[M[_]: Monad, A](m: M[A]): MonadOps[M, A] = new MonadOps[M, A](m)
}