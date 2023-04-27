package tech.backwards.fp.learn

import scala.annotation.tailrec

trait Foldable[F[_]] {
  def foldr[A, B](fa: F[A])(seed: B)(f: (A, B) => B): B
}

object Foldable {
  def apply[F[_]: Foldable]: Foldable[F] =
    implicitly

  def foldRight[A, B](xs: List[A])(seed: B)(f: (A, B) => B): B =
    xs.foldRight(seed)(f)

  object syntax {
    implicit class FoldableSyntax[F[_]: Foldable, A](fa: F[A]) {
      def foldr[B](seed: B)(f: (A, B) => B): B =
        apply[F].foldr(fa)(seed)(f)
    }

    implicit class FoldableTuple2Syntax[A](fa: (A, A)) {
      def foldr[B](seed: B)(f: (A, B) => B): B =
        foldableTuple2.foldr(fa)(seed)(f)
    }

    implicit class FoldableTuple3Syntax[A](fa: (A, A, A)) {
      def foldr[B](seed: B)(f: (A, B) => B): B =
        foldableTuple3.foldr(fa)(seed)(f)
    }
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

  /**
   * By using the "kind projector" compiler plugin the following becomes much easier:
   * {{{
   *   implicit def tuple2Foldable = new Foldable[({ type E[X] = (X, X) })# E]
   * }}}
   *
   * Another longer way is:
   * {{{
   *  type Pair[A] = (A, A)
   *
   *  implicit val foldableTuple2: Foldable[Pair] =
   *    new Foldable[Pair] {
   *      def foldr[A, B](fa: (A, A))(seed: B)(f: (A, B) => B): B = ???
   *    }
   * }}}
   * Original implementation before drying the code for many arity:
   * {{{
   *  implicit val foldableTuple2: Foldable[Lambda[X => (X, X)]] =
   *    new Foldable[Lambda[X => (X, X)]] {
   *      def foldr[A, B](fa: (A, A))(seed: B)(f: (A, B) => B): B = {
   *        println(fa.productIterator.toList.asInstanceOf[List[A]])
   *
   *        f(fa._1, f(fa._2, seed))
   *      }
   *    }
   * }}}
   */
  implicit val foldableTuple2: Foldable[Lambda[X => (X, X)]] =
    new Foldable[Lambda[X => (X, X)]] {
      def foldr[A, B](fa: (A, A))(seed: B)(f: (A, B) => B): B =
        foldRight(fa.productIterator.toList.asInstanceOf[List[A]])(seed)(f)
    }

  /**
   * Original implementation before drying the code for many arity:
   * {{{
   *  implicit val foldableTuple3: Foldable[Lambda[X => (X, X, X)]] =
   *    new Foldable[Lambda[X => (X, X, X)]] {
   *      def foldr[A, B](fa: (A, A, A))(seed: B)(f: (A, B) => B): B = {
   *        println(fa.productIterator.toList.asInstanceOf[List[A]])
   *
   *        f(fa._1, f(fa._2, f(fa._3, seed)))
   *      }
   *    }
   * }}}
   */
  implicit val foldableTuple3: Foldable[Lambda[X => (X, X, X)]] =
    new Foldable[Lambda[X => (X, X, X)]] {
      def foldr[A, B](fa: (A, A, A))(seed: B)(f: (A, B) => B): B =
        foldRight(fa.productIterator.toList.asInstanceOf[List[A]])(seed)(f)
    }
}