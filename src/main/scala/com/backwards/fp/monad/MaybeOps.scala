package com.backwards.fp.monad

import com.backwards.fp.{Just, Maybe, Nothing}

object MaybeOps {
  import com.backwards.fp.functor.MaybeOps._

  implicit val maybeMonad: Monad[Maybe] = new Monad[Maybe] {
    def pure[A](a: A): Maybe[A] = Just(a)

    def flatMap[A, B](m: Maybe[A])(f: A => Maybe[B]): Maybe[B] = m match {
      case Just(value) => f(value)
      case Nothing() => Nothing()
    }
  }
}