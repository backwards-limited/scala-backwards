package tech.backwards.fp.applicative

import tech.backwards.fp
import tech.backwards.fp.{Just, Maybe, Nothing}
import tech.backwards.fp.Nothing

object MaybeOps {
  import tech.backwards.fp.functor.MaybeOps._

  implicit val justApplicative: Applicative[Just] = new Applicative[Just] {
    def pure[A](a: A): Just[A] =
      Just(a)

    def <*>[A, R](ff: Just[A => R])(fa: Just[A]): Just[R] =
      Just(ff.value(fa.value))
  }

  implicit val nothingApplicative: Applicative[fp.Nothing] = new Applicative[fp.Nothing] {
    def pure[A](a: A): fp.Nothing[A] =
      Nothing[A]()

    def <*>[A, R](ff: Nothing[A => R])(fa: Nothing[A]): Nothing[R] =
      Nothing[R]()
  }

  implicit def maybeApplicative: Applicative[Maybe] = new Applicative[Maybe] {
    def pure[A](a: A): Maybe[A] =
      Just(a)

    def <*>[A, R](ff: Maybe[A => R])(fa: Maybe[A]): Maybe[R] =
      (ff, fa) match {
        case (Nothing(), _) => Nothing[R]()
        case (_, Nothing()) => Nothing[R]()
        case (jf@Just(_), ja@Just(_)) => justApplicative.<*>(jf)(ja)
      }
  }

  implicit def justToApplicative[A, R](just: Just[A => R])(implicit Applicative: Applicative[Just]): ApplicativeOps[Maybe, A, R] =
    new ApplicativeOps(just: Maybe[A => R])

  implicit def nothingToApplicative[A, R](nothing: Nothing[A => R])(implicit Applicative: Applicative[Nothing]): ApplicativeOps[Maybe, A, R] =
    new ApplicativeOps(nothing: Maybe[A => R])
}