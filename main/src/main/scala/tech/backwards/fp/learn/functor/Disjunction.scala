package tech.backwards.fp.learn.functor

sealed trait Disjunction[+L, +R]

final case class Left[L, R](value: L) extends Disjunction[L, R]

object Left {
  implicit def functorLeft[L]: Functor[Left[L, *]] =
    new Functor[Left[L, *]] {
      def fmap[A, B](fa: Left[L, A])(f: A => B): Left[L, B] =
        Left(fa.value)
    }
}

final case class Right[L, R](value: R) extends Disjunction[L, R]

object Right {
  implicit def functorRight[L]: Functor[Right[L, *]] =
    new Functor[Right[L, *]] {
      def fmap[A, B](fa: Right[L, A])(f: A => B): Right[L, B] =
        Right(f(fa.value))
    }
}

object Disjunction {
  /**
   * Because of using the "kind projector" compiler plugin the following becomes much easier (and also for Left and Right):
   * {{{
   *  implicit def functorDisjunction[L] =
   *    new Functor[({ type E[A] = Disjunction[L, A] })# E] {
   *      override def fmap[A, B](fa: Disjunction[L, A])(f: A => B): Disjunction[L, B] = ???
   *    }
   * }}}
   */
  implicit def functorDisjunction[L]: Functor[Disjunction[L, *]] =
    new Functor[Disjunction[L, *]] {
      def fmap[A, B](fa: Disjunction[L, A])(f: A => B): Disjunction[L, B] =
        fa match {
          case l: Left[L, A] =>
            Left.functorLeft.fmap(l)(f)

          case r: Right[L, A] =>
            Right.functorRight.fmap(r)(f)
        }
    }
}