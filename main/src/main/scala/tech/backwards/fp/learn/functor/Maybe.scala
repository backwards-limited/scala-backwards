package tech.backwards.fp.learn.functor

sealed trait Maybe[+A]

final case class Just[A](value: A) extends AnyVal

object Just {
  implicit val functorJust: Functor[Just] =
    new Functor[Just] {
      def fmap[A, B](fa: Just[A])(f: A => B): Just[B] =
        Just(f(fa.value))
    }
}

final case class Nothing[A]()

object Nothing {
  implicit val functorNothing: Functor[Nothing] =
    new Functor[Nothing] {
      def fmap[A, B](fa: Nothing[A])(f: A => B): Nothing[B] =
        apply[B]
    }

  def apply[A]: Nothing[A] =
    new Nothing[A]()
}

object Maybe {
  /*implicit val functorMaybe: Functor[Maybe] =
    new Functor[Maybe] {
      def fmap[A, B](fa: Maybe[A])(f: A => B): Maybe[B] = ???
    }*/
}