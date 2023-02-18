package tech.backwards.fp.learn

final case class Id[A](value: A) extends AnyVal

object Id {
  implicit val functorId: Functor[Id] =
    new Functor[Id] {
      def fmap[A, B](fa: Id[A])(f: A => B): Id[B] =
        Id(f(fa.value))
    }

  implicit val monadId: Monad[Id] =
    new Monad[Id] {
      def pure[A](a: A): Id[A] =
        Id(a)

      def flatMap[A, B](fa: Id[A])(f: A => Id[B]): Id[B] =
        f(fa.value)
    }

  implicit val foldableId: Foldable[Id] =
    new Foldable[Id] {
      def foldr[A, B](fa: Id[A])(seed: B)(f: (A, B) => B): B =
        f(fa.value, seed)
    }

  implicit val applicativeId: Applicative[Id] =
    new Applicative[Id] {
      def pure[A](a: A): Id[A] =
        Id(a)

      def ap[A, B](ff: Id[A => B])(fa: Id[A]): Id[B] =
        pure(ff.value(fa.value))
    }

  implicit val traversalId: Traversal[Id] =
    new Traversal[Id] {
      def traverse[G[_]: Applicative, A, B](fa: Id[A])(f: A => G[B]): G[Id[B]] =
        Applicative[G].functor.fmap(f(fa.value))(Id.apply)
    }
}