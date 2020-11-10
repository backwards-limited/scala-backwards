package com.backwards.fp.functor

import com.backwards.fp.Id

object IdOps {
  implicit val idFunctor: Functor[Id] = new Functor[Id] {
    def fmap[A, B](fa: Id[A])(f: A => B): Id[B] = Id(f(fa.value))
  }
}