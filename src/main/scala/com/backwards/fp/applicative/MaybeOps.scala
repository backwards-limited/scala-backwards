package com.backwards.fp.applicative

import com.backwards.fp.functor.Functor
import com.backwards.fp.{Just, Maybe, Nothing}

object MaybeOps {
  import com.backwards.fp.functor.MaybeOps._

  implicit val justApplicative: Applicative[Just] = new Applicative[Just] {
    def pure[A](a: A): Just[A] = ???

    def <*>[A, R](f: Just[A => R])(fa: Just[A]): Just[R] = Just(f.value(fa.value))
  }

  implicit val nothingApplicative: Applicative[Nothing] = new Applicative[Nothing] {
    def pure[A](a: A): Nothing[A] = ???

    def <*>[A, R](f: Nothing[A => R])(fa: Nothing[A]): Nothing[R] = ???
  }

  implicit def maybeApplicative: Applicative[Maybe] = new Applicative[Maybe] {
    def pure[A](a: A): Maybe[A] = ???

    def <*>[A, R](f: Maybe[A => R])(fa: Maybe[A]): Maybe[R] = (f, fa) match {
      case (Nothing(), _) => Nothing[R]()
      case (_, Nothing()) => Nothing[R]()
      case (jf @ Just(_), ja @ Just(_)) => justApplicative.<*>(jf)(ja)
    }
  }

  implicit def nothingToApplicative[A, R](nothing: Nothing[A => R])(implicit APP: Applicative[Nothing]): ApplicativeOps[Maybe, A, R] =
    new ApplicativeOps(nothing: Maybe[A => R])
}