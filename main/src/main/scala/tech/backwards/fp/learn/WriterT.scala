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

  implicit def applicativeWriterT[F[_]: Functor: Applicative, W: Monoid]: Applicative[WriterT[F, W, *]] =
    new Applicative[WriterT[F, W, *]] {
      def pure[A](a: A): WriterT[F, W, A] =
        WriterT.pure[F, W, A](a)

      def ap[A, B](ff: WriterT[F, W, A => B])(fa: WriterT[F, W, A]): WriterT[F, W, B] =
        WriterT { () =>
          val effectF: F[(W, A => B)] =
            ff.run()

          val effectA: F[(W, A)] =
            fa.run()

          Applicative[F].map2(effectF, effectA) { case ((w1, f), (w2, a)) =>
            val combinedW: W =
              Monoid[W].mappend(w1, w2)

            (combinedW, f(a))
          }
        }
    }

  implicit def monadWriterT[F[_]: Functor: Monad, W: Monoid]: Monad[WriterT[F, W, *]] =
    new Monad[WriterT[F, W, *]] {
      def pure[A](a: A): WriterT[F, W, A] =
        WriterT(() => Monad[F].pure(Monoid[W].mzero -> a))

      def flatMap[A, B](fa: WriterT[F, W, A])(f: A => WriterT[F, W, B]): WriterT[F, W, B] =
        WriterT(() =>
          Monad[F].flatMap(fa.run()) { case (w, a) =>
            Functor[F].fmap(f(a).run()) { case (ww, b) =>
              Monoid[W].mappend(w, ww) -> b
            }
          }
        )
    }
}