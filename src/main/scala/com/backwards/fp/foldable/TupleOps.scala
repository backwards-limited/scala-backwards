package com.backwards.fp.foldable

import scala.annotation.tailrec

object TupleOps {
  /**
    * Because of using the "kind projector" compiler plugin the following becomes much easier:
    * {{{
    *   implicit def tuple2Foldable = new Foldable[({ type E[X] = (X, X) })# E]
    * }}}
    */
  implicit def tuple2Foldable: Foldable[Lambda[X => (X, X)]] = new Foldable[Lambda[X => (X, X)]] {
    def foldr[A, B](fa: (A, A))(acc: B)(f: (A, B) => B): B =
      foldRight(f, acc, fa._2, fa._1)
  }

  /**
    * Because of using the "kind projector" compiler plugin the following becomes much easier:
    * {{{
    *   implicit def tuple3Foldable: Foldable[({ type E[X] = (X, X, X) })# E] = new Foldable[({ type E[X] = (X, X, X) })# E]
    * }}}
    */
  implicit def tuple3Foldable: Foldable[Lambda[X => (X, X, X)]] = new Foldable[Lambda[X => (X, X, X)]] {
    def foldr[A, B](fa: (A, A, A))(acc: B)(f: (A, B) => B): B =
      foldRight(f, acc, fa._3, fa._2, fa._1)
  }

  @tailrec
  def foldRight[A, B](f: (A, B) => B, acc: B, as: A*): B = as.toSeq match {
    case Nil => acc
    case h +: t => foldRight(f, f(h, acc), t: _*)
  }
}