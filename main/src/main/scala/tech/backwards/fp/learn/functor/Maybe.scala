package tech.backwards.fp.learn.functor

sealed trait Maybe[+A]

final case class Just[A](value: A) extends Maybe[A]

object Just {
  implicit val functorJust: Functor[Just] =
    new Functor[Just] {
      def fmap[A, B](fa: Just[A])(f: A => B): Just[B] =
        Just(f(fa.value))
    }
}

final case class Nothing[A]() extends Maybe[A]

object Nothing {
  def apply[A]: Nothing[A] =
    new Nothing[A]()

  implicit val functorNothing: Functor[Nothing] =
    new Functor[Nothing] {
      def fmap[A, B](fa: Nothing[A])(f: A => B): Nothing[B] =
        apply[B]
    }
}

object Maybe {
  implicit val functorMaybe: Functor[Maybe] =
    new Functor[Maybe] {
      def fmap[A, B](fa: Maybe[A])(f: A => B): Maybe[B] =
        fa match {
          case n: Nothing[A] =>
            Nothing.functorNothing.fmap(n)(f)

          case j: Just[A] =>
            Just.functorJust.fmap(j)(f)
        }
    }
}