package com.backwards.fp.monad

import scala.language.{higherKinds, implicitConversions}
import com.backwards.fp.functor.Functor

abstract class Monad[M[_]: Functor] { // TODO - Do we need a contraint on functor?
  def pure[A](a: A): M[A]

  def flatMap[A, B](m: M[A])(f: A => M[B]): M[B]
}

object Monad {
  def apply[M[_]: Monad]: Monad[M] = implicitly[Monad[M]]
}

class MonadOpsPure[A](a: A) {
  def pure[M[_]: Monad]: M[A] = Monad[M] pure a
}

class MonadOps[M[_]: Monad, A](m: M[A]) {
  def flatMap[B](f: A => M[B]): M[B] = Monad[M].flatMap(m)(f)

  def >>=[B](f: A => M[B]): M[B] = flatMap(f)
}

object MonadOps {
  implicit def toMonadOpsPure[A](a: A): MonadOpsPure[A] = new MonadOpsPure(a)

  implicit def toMonadOps[M[_]: Monad, A](m: M[A]): MonadOps[M, A] = new MonadOps[M, A](m)
}