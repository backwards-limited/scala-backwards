package tech.backwards.fp.learn

final case class State[S, A](run: S => (S, A)) {
  def exec(s: S): S =
    run(s)._1

  def eval(s: S): A =
    run(s)._2
}

object State {
  def put[S](s: S): State[S, Unit] =
    State(_ => s -> ())

  def get[S]: State[S, S] =
    State(s => s -> s)

  def modify[S](f: S => S): State[S, Unit] =
    State(s => f(s) -> ())

  /**
   * By using the "kind projector" compiler plugin the following becomes much easier:
   * {{{
   *  implicit def functorState[S] =
   *    new Functor[({ type E[A] = State[S, A] })# E]
   * }}}
   */
  implicit def functorState[S]: Functor[State[S, *]] =
    new Functor[State[S, *]] {
      def fmap[A, B](fa: State[S, A])(f: A => B): State[S, B] =
        State(s =>
          fa.run(s) match {
            case (s, a) => s -> f(a)
          }
        )
    }

  /**
   * By using the "kind projector" compiler plugin the following becomes much easier:
   * {{{
   *  implicit def applicativeState[S]: Applicative[({ type E[A] = State[S, A] })# E] =
   *    new Applicative[({ type E[A] = State[S, A] })# E]
   * }}}
   */
  implicit def applicativeState[S]: Applicative[State[S, *]] =
    new Applicative[State[S, *]] {
      def pure[A](a: A): State[S, A] =
        State(_ -> a)

      def ap[A, B](ff: State[S, A => B])(fa: State[S, A]): State[S, B] =
        State(s =>
          ff.run(s) match {
            case (s, f) =>
              fa.run(s) match {
                case (s, a) => s -> f(a)
              }
          }
        )
    }

  /**
   * By using the "kind projector" compiler plugin the following becomes much easier:
   * {{{
   *  implicit def monadState[S] =
   *    new Monad[({ type E[A] = State[S, A] })# E]
   * }}}
   */
  implicit def monadState[S]: Monad[State[S, *]] =
    new Monad[State[S, *]] {
      def pure[A](a: A): State[S, A] =
        State(s => s -> a)

      def flatMap[A, B](fa: State[S, A])(f: A => State[S, B]): State[S, B] =
        State(s =>
          fa.run(s) match {
            case (s, a) => f(a).run(s)
          }
        )
    }
}