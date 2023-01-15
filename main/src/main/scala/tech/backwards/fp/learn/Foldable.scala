package tech.backwards.fp.learn

import scala.annotation.tailrec

trait Foldable[F[_]] {
  def foldr[A, B](fa: F[A])(seed: B)(f: (A, B) => B): B
}

object Foldable extends FoldableImplicits {
  def apply[F[_]: Foldable]: Foldable[F] =
    implicitly

  object syntax {
    implicit class FoldableSyntax[F[_]: Foldable, A](fa: F[A]) {
      def foldr[B](seed: B)(f: (A, B) => B): B =
        apply[F].foldr(fa)(seed)(f)
    }
  }
}

sealed trait FoldableImplicits {
  implicit val foldableId: Foldable[Id] =
    new Foldable[Id] {
      def foldr[A, B](fa: Id[A])(seed: B)(f: (A, B) => B): B =
        f(fa.value, seed)
    }

  implicit val foldableList: Foldable[List] =
    new Foldable[List] {
      def foldr[A, B](fa: List[A])(seed: B)(f: (A, B) => B): B = {
        @tailrec
        def go(fa: List[A], acc: B): B =
          (fa, acc) match {
            case (Nil, acc) => acc
            case (x :: xs, acc) => go(xs, f(x, acc))
          }

        go(fa.reverse, seed)
      }
    }
}