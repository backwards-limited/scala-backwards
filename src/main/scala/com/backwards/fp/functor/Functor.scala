package com.backwards.fp.functor

import scala.language.{higherKinds, implicitConversions}

trait Functor[F[_]] {
  def fmap[A, B](fa: F[A])(f: A => B): F[B]
}

object Functor {
  def apply[F[_]: Functor]: Functor[F] = implicitly
}

class FunctorOps[F[_]: Functor, A](fa: F[A]) {
  def fmap[B](f: A => B): F[B] = Functor[F].fmap(fa)(f)

  /**
    * To be compatible with "for"
    * @param f A => B function
    * @tparam B B from an A
    * @return F[B]
    */
  def map[B](f: A => B): F[B] = Functor[F].fmap(fa)(f)
}

class FunctorFunctionOps[A, B](f: A => B) {
  def `<$>`[F[_]: Functor](fa: F[A]): F[B] =
    Functor[F].fmap(fa)(f)
}

object FunctorOps {
  implicit def toFunctorOps[F[_]: Functor, A](fa: F[A]): FunctorOps[F, A] =
    new FunctorOps(fa)

  implicit def toFunctorFunctionOps[A, B](f: A => B): FunctorFunctionOps[A, B] =
    new FunctorFunctionOps[A, B](f)
}