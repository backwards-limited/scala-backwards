package tech.backwards.fp.learn.functor

final case class Writer[W, A](run: () => (W, A))

object Writer {
  def tell[W](w: W): Writer[W, Unit] =
    Writer(() => w -> ())

  implicit def functorWriter[W]: Functor[Writer[W, *]] =
    new Functor[Writer[W, *]] {
      def fmap[A, B](fa: Writer[W, A])(f: A => B): Writer[W, B] =
        fa.run() match {
          case (w, a) => Writer(() => w -> f(a))
        }
    }
}