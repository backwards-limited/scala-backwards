package tech.backwards.fp.functor

import tech.backwards.fp.Id

object IdOps {
  implicit val idFunctor: Functor[Id] =
    new Functor[Id] {
      def fmap[A, B](fa: Id[A])(f: A => B): Id[B] = Id(f(fa.value))
    }
}