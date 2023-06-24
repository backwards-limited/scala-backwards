package tech.backwards.fp.learn

final case class MaybeT[F[_], A](value: F[Maybe[A]])

object MaybeT {
  def pure[F[_]: Applicative, A](a: A): MaybeT[F, A] =
    MaybeT(Applicative[F].pure(Just(a)))

  def lift[F[_]: Functor, A](fa: F[A]): MaybeT[F, A] =
    MaybeT(Functor[F].fmap(fa)(Just.apply))

  implicit def functorMaybeT[F[_]: Functor]: Functor[MaybeT[F, *]] = {
    import tech.backwards.fp.learn.Functor.syntax._

    new Functor[MaybeT[F, *]] {
      def fmap[A, B](fa: MaybeT[F, A])(f: A => B): MaybeT[F, B] =
        MaybeT(Functor[F].fmap(fa.value)(_.fmap(f)))
    }
  }

  implicit def applicateMaybeT[F[_]: Functor: Applicative]: Applicative[MaybeT[F, *]] =
    new Applicative[MaybeT[F, *]] {
      import tech.backwards.fp.learn.Functor.syntax._
      import tech.backwards.fp.learn.Applicative.syntax._

      def pure[A](a: A): MaybeT[F, A] =
        MaybeT(Applicative[F].pure(Just(a)))

      def ap[A, B](ff: MaybeT[F, A => B])(fa: MaybeT[F, A]): MaybeT[F, B] =
        MaybeT(ff.value `<$>` ((ff: Maybe[A => B]) => (fa: Maybe[A]) => ff <*> fa) <*> fa.value)
    }

  implicit def monadMaybeT[F[_]: Functor: Monad]: Monad[MaybeT[F, *]] =
    new Monad[MaybeT[F, *]] {
      import tech.backwards.fp.learn.Monad.syntax._

      def pure[A](a: A): MaybeT[F, A] =
        MaybeT(Monad[F].pure(Just(a)))

      def flatMap[A, B](fa: MaybeT[F, A])(f: A => MaybeT[F, B]): MaybeT[F, B] =
        MaybeT(
          fa.value.flatMap {
            case Just(a) => f(a).value
            case Nothing() => Monad[F].pure(Nothing[B])
          }
        )
    }
}