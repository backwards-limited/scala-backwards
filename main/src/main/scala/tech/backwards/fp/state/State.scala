package tech.backwards.fp.state

final case class State[S, A](run: S => (S, A)) {
  def exec(s: S): S =
    run(s)._1

  def eval(s: S): A =
    run(s)._2
}

object State {
  def put[S](s: S): State[S, Unit] =
    State(_ => (s, ()))

  def get[S]: State[S, S] =
    State(s => (s, s))

  def modify[S](f: S => S): State[S, Unit] =
    State(s => (f(s), ()))
}