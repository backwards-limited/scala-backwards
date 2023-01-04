package tech.backwards.fp.learn

final case class Writer[W, A](run: () => (W, A))

object Writer {
  def tell[W](w: W): Writer[W, Unit] =
    Writer(() => w -> ())

  def tell[W: Monoid]: Writer[W, Unit] =
    Writer(() => Monoid[W].mzero -> ())

  /**
   * Because of using the "kind projector" compiler plugin the following becomes much easier:
   * {{{
   *  implicit def functorWriter[W] =
   *    new Functor[({ type E[A] = Writer[W, A] })# E]
   * }}}
   */
  implicit def functorWriter[W]: Functor[Writer[W, *]] =
    new Functor[Writer[W, *]] {
      def fmap[A, B](fa: Writer[W, A])(f: A => B): Writer[W, B] =
        fa.run() match {
          case (w, a) => Writer(() => w -> f(a))
        }
    }

  implicit def monadWriter[W: Monoid]: Monad[Writer[W, *]] =
    new Monad[Writer[W, *]] {
      import tech.backwards.fp.learn.Functor.syntax._
      import tech.backwards.fp.learn.Monoid.syntax._

      def pure[A](a: A): Writer[W, A] =
        tell(Monoid[W].mzero).map(_ => a)

      def flatMap[A, B](fa: Writer[W, A])(f: A => Writer[W, B]): Writer[W, B] = {
        val (w, a) = fa.run()

        val (w2, b) = f(a).run()

        tell(w |+| w2).as(b)
      }
    }
}