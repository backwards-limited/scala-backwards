package tech.backwards.fp.learn

/**
 * Cats declares the following as:
 * {{{
 *   final case class IdT[F[_], A](value: F[A])
 * }}}
 */
final case class IdT[F[_], A](value: F[Id[A]])

object IdT {
  def pure[F[_]: Applicative, A](a: A): IdT[F, A] =
    IdT(Applicative[F].pure(Id(a)))

  implicit def functorIdT[F[_]]: Functor[IdT[F, *]] =
    new Functor[IdT[F, *]] {
      def fmap[A, B](fa: IdT[F, A])(f: A => B): IdT[F, B] = ???
    }

  implicit def applicativeIdT[F[_]: Applicative]: Applicative[IdT[F, *]] =
    new Applicative[IdT[F, *]] {
      def pure[A](a: A): IdT[F, A] =
        IdT(Applicative[F].pure(Id(a)))

      def ap[A, B](ff: IdT[F, A => B])(fa: IdT[F, A]): IdT[F, B] = ???
    }
}