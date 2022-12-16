package tech.backwards.fp.functor

import tech.backwards.fp
import tech.backwards.fp.{Just, Maybe, Nothing}
import tech.backwards.fp.Nothing

object MaybeOps {
  implicit val justFunctor: Functor[Just] =
    new Functor[Just] {
      def fmap[A, B](fa: Just[A])(f: A => B): Just[B] = Just(f(fa.value))
    }

  implicit val nothingFunctor: Functor[fp.Nothing] =
    new Functor[fp.Nothing] {
      def fmap[A, B](fa: fp.Nothing[A])(f: A => B): fp.Nothing[B] = Nothing[B]()
    }

  implicit val maybeFunctor: Functor[Maybe] =
    new Functor[Maybe] {
      def fmap[A, B](fa: Maybe[A])(f: A => B): Maybe[B] = fa match {
        case j @ Just(_) => justFunctor.fmap(j)(f)
        case n @ Nothing() => nothingFunctor.fmap(n)(f)
      }
    }

  def just[A](x: A): Maybe[A] =
    Just(x)

  def nothing[A]: Maybe[A] =
    Nothing[A]()
}