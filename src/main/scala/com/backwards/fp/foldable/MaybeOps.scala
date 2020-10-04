package com.backwards.fp.foldable

import com.backwards.fp.{Just, Maybe, Nothing}
import com.backwards.fp.foldable.Backwards._

object MaybeOps {
  implicit val maybeFoldable: Foldable[Maybe] = new Foldable[Maybe] {
    def foldr[A, B](fa: Maybe[A])(acc: B)(f: (A, B) => B): B = fa match {
      case j @ Just(_) => justFoldable.foldr(j)(acc)(f)
      case n @ Nothing() => nothingFoldable.foldr(n)(acc)(f)
    }
  }

  implicit val justFoldable: Foldable[Just] = new Foldable[Just] {
    def foldr[A, B](fa: Just[A])(acc: B)(f: (A, B) => B): B = f(fa.value, acc)
  }

  implicit val nothingFoldable: Foldable[Nothing] = new Foldable[Nothing] {
    def foldr[A, B](fa: Nothing[A])(acc: B)(f: (A, B) => B): B = acc
  }
}