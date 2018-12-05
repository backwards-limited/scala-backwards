package com.backwards.fp.functor

import com.backwards.fp.{Just, Maybe, Nothing}

object MaybeOps {
  implicit val justFunctor: Functor[Just] = new Functor[Just] {
    def fmap[A, B](fa: Just[A])(f: A => B): Just[B] = Just(f(fa.value))
  }

  implicit val nothingFunctor: Functor[Nothing] = new Functor[Nothing] {
    def fmap[A, B](fa: Nothing[A])(f: A => B): Nothing[B] = Nothing[B]()
  }

  implicit val maybeFunctor: Functor[Maybe] = new Functor[Maybe] {
    def fmap[A, B](fa: Maybe[A])(f: A => B): Maybe[B] = fa match {
      case j @ Just(_) => justFunctor.fmap(j)(f)
      case n @ Nothing() => nothingFunctor.fmap(n)(f)
    }
  }

  def just[A](x: A): Maybe[A] = Just(x)

  def nothing[A]: Maybe[A] = Nothing[A]()
}