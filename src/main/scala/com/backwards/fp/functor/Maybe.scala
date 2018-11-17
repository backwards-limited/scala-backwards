package com.backwards.fp.functor

sealed trait Maybe[+A]

// ONE SOLUTION
object Maybe {
  implicit val maybeFunctor: Functor[Maybe] = new Functor[Maybe] {
    def fmap[A, B](fa: Maybe[A])(f: A => B): Maybe[B] = fa match {
      case j @ Just(_) => Just.justFunctor.fmap(j)(f)
      case n @ Nothing() => Nothing.nothingFunctor.fmap(n)(f)
    }
  }
}

final case class Just[A](value: A) extends Maybe[A]

object Just {
  implicit val justFunctor: Functor[Just] = new Functor[Just] {
    def fmap[A, B](fa: Just[A])(f: A => B): Just[B] = Just(f(fa.value))
  }
}

final case class Nothing[A]() extends Maybe[A]
// final case object Nothing extends Maybe[Nothing] <- actual Scala Nothing

object Nothing {
  implicit val nothingFunctor: Functor[Nothing] = new Functor[Nothing] {
    def fmap[A, B](fa: Nothing[A])(f: A => B): Nothing[B] = Nothing[B]()
  }
}

/*
ANOTHER SOLUTION

object Maybe {
  implicit val maybeFunctor: Functor[Maybe] = new Functor[Maybe] {
    def fmap[A, B](fa: Maybe[A])(f: A => B): Maybe[B] = fa match {
      case Just(a) => Just(f(a))
      case Nothing() => Nothing[B]()
    }
  }

  def nothing[A](): Maybe[A] = Nothing[A]()

  def just[A](x: A): Maybe[A] = Just(x)
}

final case class Just[A](value: A) extends Maybe[A]

case class Nothing[A]() extends Maybe[A]
*/