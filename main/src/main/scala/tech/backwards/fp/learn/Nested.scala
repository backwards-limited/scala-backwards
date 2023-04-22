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

  /**
   * By using the "kind projector" compiler plugin the following becomes much easier:
   * {{{
   *  implicit def applicativeNested[F[_]: Applicative: Functor, G[_]: Applicative: Functor] =
   *    new Applicative[({ type E[A] = Nested[F, G, A] })# E]
   * }}}
   */
  implicit def applicativeNested[F[_]: Applicative: Functor, G[_]: Applicative: Functor]: Applicative[Nested[F, G, *]] =
    new Applicative[Nested[F, G, *]] {
      import tech.backwards.fp.learn.Functor.syntax._
      import tech.backwards.fp.learn.Applicative.syntax._
      import tech.backwards.fp.learn.Applicative.syntax.function._

      def pure[A](a: A): Nested[F, G, A] =
        Nested(a.pure[G].pure[F]) // i.e. Nested(Applicative[F].pure(Applicative[G].pure(a)))

      def ap[A, B](ff: Nested[F, G, A => B])(fa: Nested[F, G, A]): Nested[F, G, B] =
        Nested(ff.value `<$>` ((gab: G[A => B]) => (ga: G[A]) => gab <*> ga) <*> fa.value)
    }
}