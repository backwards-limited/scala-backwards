package com.backwards.fp.foldable

import com.backwards.fp.Id

object IdOps {
  implicit val idFoldable: Foldable[Id] = new Foldable[Id] {
    def foldr[A, B](fa: Id[A])(acc: B)(f: (A, B) => B): B = f(fa.value, acc)
  }
}