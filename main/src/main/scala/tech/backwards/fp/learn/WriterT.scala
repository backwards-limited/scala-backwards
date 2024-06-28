package tech.backwards.fp.learn

final case class WriterT[F[_], W, A](run: () => F[(W, A)])

object WriterT {
  def pure[F[_]: Applicative, W: Monoid, A](a: A): WriterT[F, W, A] =
    WriterT(() => Applicative[F].pure(Monoid[W].mzero -> a))

  def tell[F[_]: Applicative, W](w: W): WriterT[F, W, Unit] =
    WriterT(() => Applicative[F].pure(w -> ()))

  def lift[F[_]: Functor, W: Monoid, A](fa: F[A]): WriterT[F, W, A] =
    WriterT(() => Functor[F].fmap(fa)(Monoid[W].mzero -> _))

  implicit def functorWriterT[F[_]: Functor, W]: Functor[WriterT[F, W, *]] =
    new Functor[WriterT[F, W, *]] {
      def fmap[A, B](fa: WriterT[F, W, A])(f: A => B): WriterT[F, W, B] =
        WriterT(() => Functor[F].fmap(fa.run()) { case (w, a) => w -> f(a) })
    }

  /*implicit def applicativeWriterT[F[_]: Functor: Applicative]: Applicative[WriterT[F, *]] =
    new Applicative[WriterT[F, *]] {
      import tech.backwards.fp.learn.Applicative.syntax._
      import tech.backwards.fp.learn.Functor.syntax._

      def pure[A](a: A): WriterT[F, A] =
        WriterT(Applicative[F].pure(Just(a)))

      def ap[A, B](ff: WriterT[F, A => B])(fa: WriterT[F, A]): WriterT[F, B] =
        WriterT(
          ff.value `<$>` ((ff: Maybe[A => B]) => (fa: Maybe[A]) => ff <*> fa) <*> fa.value
        )
    }*/

  /*implicit def monadWriterT[F[_]: Functor: Monad]: Monad[WriterT[F, *]] =
    new Monad[WriterT[F, *]] {
      import tech.backwards.fp.learn.Monad.syntax._

      def pure[A](a: A): WriterT[F, A] =
        WriterT(Monad[F].pure(Just(a)))

      def flatMap[A, B](fa: WriterT[F, A])(f: A => WriterT[F, B]): WriterT[F, B] =
        WriterT(
          fa.value.flatMap {
            case Just(a)   => f(a).value
            case Nothing() => Monad[F].pure(Nothing[B])
          }
        )
    }*/
}