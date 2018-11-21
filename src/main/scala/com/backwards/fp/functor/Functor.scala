package com.backwards.fp.functor

import scala.language.{higherKinds, implicitConversions}

trait Functor[F[_]] {
  def fmap[A, B](fa: F[A])(f: A => B): F[B]
}

object Functor {
  def apply[F[_]: Functor]: Functor[F] = implicitly[Functor[F]]
}

class FunctorOps[F[_]: Functor, A](a: F[A]) {
  def fmap[B](f: A => B): F[B] = Functor[F].fmap(a)(f)

  /**
    * To be compatible with "for"
    * @param f A => B function
    * @tparam B B from an A
    * @return F[B]
    */
  def map[B](f: A => B): F[B] = Functor[F].fmap(a)(f)
}

class FunctionFunctorOps[A, B](f: A => B) {
  def `<$>`[F[_]: Functor](a: F[A]): F[B] =
    Functor[F].fmap(a)(f)
}

object FunctorOps {
  implicit def toFunctorOps[F[_]: Functor, A](f: F[A]): FunctorOps[F, A] =
    new FunctorOps(f)

  implicit def toFunctionFunctorOps[A, B](f: A => B): FunctionFunctorOps[A, B] =
    new FunctionFunctorOps[A, B](f)
}