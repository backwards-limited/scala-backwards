package tech.backwards.fp.applicative

import tech.backwards.fp.Id

object IdOps {
  import tech.backwards.fp.functor.IdOps._

  implicit val idApplicative: Applicative[Id] =
    new Applicative[Id] {
      def pure[A](a: A): Id[A] =
        Id(a)

      def <*>[A, R](ff: Id[A => R])(fa: Id[A]): Id[R] =
        Id(ff.value(fa.value))
    }
}