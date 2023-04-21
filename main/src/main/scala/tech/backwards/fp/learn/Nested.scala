package tech.backwards.fp.learn

final case class Nested[F[_], G[_], A](value: F[G[A]])

object Nested {
  /**
   * By using the "kind projector" compiler plugin the following becomes much easier:
   * {{{
   *  implicit def functorNested[F[_]: Functor, G[_]: Functor] =
   *    new Functor[({ type E[A] = Nested[F, G, A] })# E]
   * }}}
   */
  implicit def functorNested[F[_]: Functor, G[_]: Functor]: Functor[Nested[F, G, *]] =
    new Functor[Nested[F, G, *]] {
      def fmap[A, B](fa: Nested[F, G, A])(f: A => B): Nested[F, G, B] =
        Nested(Functor[F].fmap(fa.value)(ga => Functor[G].fmap(ga)(a => f(a))))
    }
}