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
}