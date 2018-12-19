package com.backwards.fp.applicative

import scala.language.{higherKinds, implicitConversions}
import com.backwards.fp.functor.Functor

/**
  * Why not:
  * def <*>[A, B](f: F[A => B])(fa: F[A]): F[B]
  *
  * because of currying, B may not actually be the "end of the line".
  * B itself maybe a function, just of one less argument, until we get down to 1 argument and then a result R
  */
abstract class Applicative[F[_]: Functor] {
  def pure[A](a: A): F[A]

  def <*>[A, R](ff: F[A => R])(fa: F[A]): F[R]
}

object Applicative {
  def apply[F[_]: Applicative]: Applicative[F] = implicitly
}

class ApplicativeOps[F[_], A, R](ff: F[A => R]) {
  def <*>(fa: F[A])(implicit APPLICATIVE: Applicative[F]): F[R] = APPLICATIVE.<*>(ff)(fa)
}

object ApplicativeOps {
  implicit def toApplicativeOps[F[_]: Applicative, A, R](ff: F[A => R]): ApplicativeOps[F, A, R] =
    new ApplicativeOps[F, A, R](ff)
}