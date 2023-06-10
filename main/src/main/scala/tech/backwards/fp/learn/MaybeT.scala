package tech.backwards.fp.learn

final case class MaybeT[F[_], A](value: F[Maybe[A]])