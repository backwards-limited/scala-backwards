package com.backwards.fp.applicative

import scala.language.implicitConversions
import com.backwards.fp.applicative.Backwards._
import com.backwards.fp.functor.Backwards.{Functor, FunctorFunctionOps}
import com.backwards.fp.{Disjunction, LeftDisjunction, RightDisjunction}

object DisjunctionOps {
  implicit def disjunctionApplicative[L](implicit Functor: Functor[Disjunction[L, *]]): Applicative[Disjunction[L, *]] = new Applicative[Disjunction[L, *]] {
    def pure[A](a: A): Disjunction[L, A] = RightDisjunction(a)

    def <*>[A, R](ff: Disjunction[L, A => R])(fa: Disjunction[L, A]): Disjunction[L, R] = ff match {
      case LeftDisjunction(value) => LeftDisjunction(value)
      case RightDisjunction(f) => Functor.fmap(fa)(f)
    }
  }

  implicit def toApplicativeOps[L, A, R](disjunction: Disjunction[L, A => R])(implicit Applicative: Applicative[Disjunction[L, *]]): ApplicativeOps[Disjunction[L, *], A, R] =
    new ApplicativeOps[Disjunction[L, *], A, R](disjunction)

  implicit def toFunctorFunctionOps[A, B](f: A => B): FunctorFunctionOps[A, B] =
    new FunctorFunctionOps[A, B](f)
}