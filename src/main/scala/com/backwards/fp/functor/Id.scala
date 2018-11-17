package com.backwards.fp.functor

final case class Id[A](value: A) extends AnyVal

object Id {
  implicit val idFunctor: Functor[Id] = new Functor[Id] {
    def fmap[A, B](fa: Id[A])(f: A => B): Id[B] = Id(f(fa.value))
  }
}