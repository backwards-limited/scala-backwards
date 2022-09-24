package tech.backwards.bookofmonads.ch10

import cats.Functor
import cats.implicits._
import munit.FunSuite

/**
 * Functor composition
 */
class Ex1Suite extends FunSuite {
  test("Functor composition") {
    /*
    If we know that both f and g are functors, we can make a new functor out of their composition.

    Haskell:
    (a -> b) -> (f (g a)) -> (f (g b))

    Scala:
    (A => B) => F[G[A]] => F[G[B]]
    */
  }
}