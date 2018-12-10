package com.backwards.fp.applicative

import scala.language.higherKinds
import com.backwards.fp.{Disjunction, LeftDisjunction, RightDisjunction}
import com.backwards.fp.functor.Functor

object DisjunctionOps {
  import com.backwards.fp.functor.DisjunctionOps._

  implicit def rightDisjunctionApplicative[L] = new Applicative[RightDisjunction[L, ?]] {
    def pure[A](a: A): RightDisjunction[L, A] = ???

    def <*>[A, R](ff: RightDisjunction[L, A => R])(fa: RightDisjunction[L, A]): RightDisjunction[L, R] = ???
  }

  /*implicit def disjunctionApplicative[D[_, _] <: Disjunction[_, _], L](implicit F: Functor[D[L, ?]]): Applicative[D[L, ?]] = new Applicative[D[L, ?]] {
    def pure[A](a: A): D[L, A] = ???

    def <*>[A, R](ff: D[L, A => R])(fa: D[L, A]): D[L, R] = ???
  }

  implicit def toApplicativeOps[D[_, _] <: Disjunction[_, _], L, A, R](disjunction: D[L, A => R])(implicit APP: Applicative[D[L, ?]]) =
    new ApplicativeOps[D[L, ?], A, R](disjunction)*/

  implicit def disjunctionApplicative[L](implicit F: Functor[Disjunction[L, ?]]): Applicative[Disjunction[L, ?]] = new Applicative[Disjunction[L, ?]] {
    def pure[A](a: A): Disjunction[L, A] = ???

    def <*>[A, R](ff: Disjunction[L, A => R])(fa: Disjunction[L, A]): Disjunction[L, R] = fa match {
      case LeftDisjunction(l) => LeftDisjunction[L, R](l)
      case RightDisjunction(value) => ff match {
        case LeftDisjunction(l) => LeftDisjunction(l)
        case RightDisjunction(f) => RightDisjunction(f(value))
      }
    }
  }

  implicit def toApplicativeOps[L, A, R](disjunction: Disjunction[L, A => R])(implicit APP: Applicative[Disjunction[L, ?]]) =
    new ApplicativeOps[Disjunction[L, ?], A, R](disjunction)
}