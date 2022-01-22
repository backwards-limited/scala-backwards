package tech.backwards.fp.monad

import tech.backwards.fp.Id

object IdOps {
  import tech.backwards.fp.functor.IdOps._

  implicit val idMonad: Monad[Id] =
    new Monad[Id] {
      def pure[A](a: A): Id[A] = Id(a)

      def flatMap[A, B](m: Id[A])(f: A => Id[B]): Id[B] = f(m.value)
    }
}