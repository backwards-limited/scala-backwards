package tech.backwards.fp.foldable

import tech.backwards.fp.Id

object IdOps {
  implicit val idFoldable: Foldable[Id] =
    new Foldable[Id] {
      def foldr[A, B](fa: Id[A])(acc: B)(f: (A, B) => B): B = f(fa.value, acc)
    }
}