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
   * Because of using the "kind projector" compiler plugin the following becomes much easier:
   * {{{
   *  implicit def functorState[S] =
   *    new Functor[({ type E[A] = State[S, A] })# E]
   * }}}
   */
  implicit def functorState[S]: Functor[State[S, *]] =
    new Functor[State[S, *]] {
      def fmap[A, B](fa: State[S, A])(f: A => B): State[S, B] =
        State { s =>
          val (ss, a) = fa.run(s)
          ss -> f(a)
        }
    }
}