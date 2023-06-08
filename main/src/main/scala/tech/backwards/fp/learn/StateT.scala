package tech.backwards.fp.learn

/**
 * Cats declares the following as:
 * {{{
 *   final class IndexedStateT[F[_], SA, SB, A](val runF: F[SA => F[(SB, A)]])
 * }}}
 */
final case class StateT[F[_], S, A](run: S => F[(S, A)])

object StateT { self =>
  def lift[F[_]: Functor: Monad, S, A](fa: F[A]): StateT[F, S, A] =
    StateT((s: S) => Functor[F].fmap(fa)(a => s -> a))

  def modify[F[_]: Monad, A](f: A => A): StateT[F, A, Unit] =
    StateT((a: A) => Monad[F].pure(f(a) -> ()))

  implicit def functorStateT[F[_]: Functor, S]: Functor[StateT[F, S, *]] =
    new Functor[StateT[F, S, *]] {
      def fmap[A, B](fa: StateT[F, S, A])(f: A => B): StateT[F, S, B] =
        StateT(s => Functor[F].fmap(fa.run(s)) { case (s, a) => s -> f(a) })
    }

  implicit def monadStateT[F[_]: Functor: Monad, S]: Monad[StateT[F, S, *]] = {
    import tech.backwards.fp.learn.Monad.syntax._

    new Monad[StateT[F, S, *]] {
      def pure[A](a: A): StateT[F, S, A] =
        StateT(s => Monad[F].pure(s -> a))

      def flatMap[A, B](fa: StateT[F, S, A])(f: A => StateT[F, S, B]): StateT[F, S, B] =
        StateT(fa.run.andThen(_ >>= { case (s, a) => f(a).run(s) }))
    }
  }
}