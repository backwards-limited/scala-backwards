package tech.backwards.fp.learn

final case class Writer[W, A](run: () => (W, A))

object Writer {
  def apply[W, A](wa: (W, A)): Writer[W, A] =
    Writer(() => wa)

  def writer[W: Monoid]: Writer[W, Unit] =
    Writer(() => Monoid[W].mzero -> ())

  def tell[W](w: W): Writer[W, Unit] =
    Writer(() => w -> ())

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

  /**
   * Because of using the "kind projector" compiler plugin the following becomes much easier:
   * {{{
   *  implicit def monadWriter[W: Monoid] =
   *    new Monad[({ type E[A] = Writer[W, A] })# E]
   * }}}
   */
  implicit def monadWriter[W: Monoid]: Monad[Writer[W, *]] =
    new Monad[Writer[W, *]] {
      import tech.backwards.fp.learn.Functor.syntax._
      import tech.backwards.fp.learn.Monoid.syntax._

      def pure[A](a: A): Writer[W, A] =
        writer[W].as(a)

      def flatMap[A, B](fa: Writer[W, A])(f: A => Writer[W, B]): Writer[W, B] = {
        val (w,  a) = fa.run()
        val (w2, b) = f(a).run()

        tell(w |+| w2).as(b)
      }
    }

  /**
   * Because of using the "kind projector" compiler plugin the following becomes much easier:
   * {{{
   *  implicit def applicativeWriter[W: Monoid] =
   *    new Applicative[({ type E[A] = Writer[W, A] })# E]
   * }}}
   */
  implicit def applicativeWriter[W: Monoid]: Applicative[Writer[W, *]] =
    new Applicative[Writer[W, *]] {
      import tech.backwards.fp.learn.Functor.syntax._
      import tech.backwards.fp.learn.Monoid.syntax._

      def pure[A](a: A): Writer[W, A] =
        writer[W].as(a)

      def ap[A, B](ff: Writer[W, A => B])(fa: Writer[W, A]): Writer[W, B] = {
        val (w, f) = ff.run()
        val (w2, a) = fa.run()

        tell(w |+| w2).as(f(a))
      }
    }
}