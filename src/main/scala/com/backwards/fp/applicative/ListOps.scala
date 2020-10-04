package com.backwards.fp.applicative

object ListOps {
  import com.backwards.fp.functor.ListOps._

  implicit val listApplicative: Applicative[List] = new Applicative[List] {
    def pure[A](a: A): List[A] = List(a)

    def <*>[A, R](ff: List[A => R])(fa: List[A]): List[R] = for {
      f <- ff
      a <- fa
    } yield f(a)
  }
}