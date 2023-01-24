package tech.backwards.fp.learn

sealed trait Maybe[+A]

final case class Just[A] private(value: A) extends Maybe[A]

object Just {
  def apply[A](a: A): Maybe[A] =
    new Just(a)
}

final case class Nothing[A] private() extends Maybe[A]

object Nothing {
  def apply[A]: Maybe[A] =
    new Nothing[A]()
}

object Maybe {
  implicit val functorMaybe: Functor[Maybe] =
    new Functor[Maybe] {
      def fmap[A, B](fa: Maybe[A])(f: A => B): Maybe[B] =
        fa match {
          case Nothing() =>
            Nothing[B]

          case Just(a) =>
            Just(f(a))
        }
    }

  implicit val monadMaybe: Monad[Maybe] =
    new Monad[Maybe] {
      def pure[A](a: A): Maybe[A] =
        Just(a)

      def flatMap[A, B](fa: Maybe[A])(f: A => Maybe[B]): Maybe[B] =
        fa match {
          case Nothing() =>
            Nothing[B]

          case Just(a) =>
            f(a)
        }
    }

  implicit val foldableMaybe: Foldable[Maybe] =
    new Foldable[Maybe] {
      def foldr[A, B](fa: Maybe[A])(seed: B)(f: (A, B) => B): B =
        fa match {
          case Nothing() =>
            seed

          case Just(a) =>
            f(a, seed)
        }
    }
}