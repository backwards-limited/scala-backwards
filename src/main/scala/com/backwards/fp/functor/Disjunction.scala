package com.backwards.fp.functor

import scala.language.higherKinds

sealed trait Disjunction[+L, +R]

object Disjunction {
  /**
    * Because of using the "kind projector" compiler plugin the following becomes much easier:
    * {{{
    *   implicit def disjunctionToFunctorOps[L, R, D[L, R] <: Disjunction[L, R]](disjunction: D[L, R])
    *                                                                           (implicit functor: Functor[({ type E[A] = D[L, A] })# E]) =
    *     new FunctorOps[R, ({ type E[A] = D[L, A] })# E](disjunction)
    * }}}
    */
  implicit def toFunctorOps[L, R, D[L, R] <: Disjunction[L, R]](disjunction: D[L, R])(implicit functor: Functor[D[L, ?]]) =
    new FunctorOps[R, D[L, ?]](disjunction)

  /**
    * Because of using the "kind projector" compiler plugin the following becomes much easier:
    * {{{
    *   implicit def disjunctionFunctor[L] = new Functor[({ type E[A] = Disjunction[L, A] })# E] {
    *     override def fmap[A, B](a: Disjunction[L, A])(f: A => B): Disjunction[L, B] = ???
    *   }
    * }}}
    */
  implicit def disjunctionFunctor[L] = new Functor[Disjunction[L, ?]] {
    override def fmap[A, B](d: Disjunction[L, A])(f: A => B): Disjunction[L, B] = d match {
      case l @ LeftDisjunction(_) => LeftDisjunction.leftDisjunctionFunctor.fmap(l)(f)
      case RightDisjunction(value) => RightDisjunction(f(value))
    }
  }

  def left[L, R](lv: L): Disjunction[L, R] = LeftDisjunction[L, R](lv)

  def right[L, R](rv: R): Disjunction[L, R] = RightDisjunction[L, R](rv)
}

final case class LeftDisjunction[L, R](value: L) extends Disjunction[L, R]

object LeftDisjunction {
  implicit def leftDisjunctionFunctor[L, R] = new Functor[LeftDisjunction[L, ?]] {
    def fmap[A, B](fa: LeftDisjunction[L, A])(f: A => B): LeftDisjunction[L, B] = LeftDisjunction[L, B](fa.value)
  }
}

final case class RightDisjunction[L, R](value: R) extends Disjunction[L, R]

object RightDisjunction {
  implicit def rightDisjunctionFunctor[L, R] = new Functor[RightDisjunction[L, ?]] {
    def fmap[A, B](fa: RightDisjunction[L, A])(f: A => B): RightDisjunction[L, B] = RightDisjunction(f(fa.value))
  }
}