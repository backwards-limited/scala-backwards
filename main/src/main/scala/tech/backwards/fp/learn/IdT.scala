package tech.backwards.fp.learn

/**
 * Cats declares the following as:
 * {{{
 *   final case class IdT[F[_], A](value: F[A])
 * }}}
 */
final case class IdT[F[_], A](value: F[Id[A]])

object IdT { self =>
  def pure[F[_]: Applicative, A](a: A): IdT[F, A] =
    IdT(Applicative[F].pure(Id(a)))

  def lift[F[_]: Functor, A](fa: F[A]): IdT[F, A] =
    IdT(Functor[F].fmap(fa)(Id.apply))

  implicit def functorIdT[F[_]: Functor]: Functor[IdT[F, *]] =
    new Functor[IdT[F, *]] {
      def fmap[A, B](fa: IdT[F, A])(f: A => B): IdT[F, B] =
        IdT(Functor[F].fmap(fa.value)(a => Id(f(a.value))))
    }

  implicit def applicativeIdT[F[_]: Functor: Applicative]: Applicative[IdT[F, *]] =
    new Applicative[IdT[F, *]] {
      import tech.backwards.fp.learn.Functor.syntax._
      import tech.backwards.fp.learn.Applicative.syntax._

      def pure[A](a: A): IdT[F, A] =
        IdT(Applicative[F].pure(Id(a)))

      def ap[A, B](ff: IdT[F, A => B])(fa: IdT[F, A]): IdT[F, B] =
        IdT(ff.value `<$>` ((ff: Id[A => B]) => (fa: Id[A]) => ff <*> fa) <*> fa.value)
    }

  implicit def monadIdT[F[_]: Functor: Monad]: Monad[IdT[F, *]] =
    new Monad[IdT[F, *]] {
      def pure[A](a: A): IdT[F, A] =
        IdT(Monad[F].pure(Id(a)))

      def flatMap[A, B](fa: IdT[F, A])(f: A => IdT[F, B]): IdT[F, B] =
        IdT(Monad[F].flatMap(fa.value)(idA => f(idA.value).value))
    }
}