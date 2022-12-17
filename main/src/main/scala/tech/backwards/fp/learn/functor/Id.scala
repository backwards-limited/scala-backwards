package tech.backwards.fp.learn.functor

final case class Id[A](value: A) extends AnyVal

object Id {
  implicit val functorId: Functor[Id] =
    new Functor[Id] {
      def fmap[A, B](fa: Id[A])(f: A => B): Id[B] =
        Id(f(fa.value))
    }
}