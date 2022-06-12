package tech.backwards.bookofmonads.ch0

import munit._

/**
 * Abstraction over type constructors is also known as higher-kinded abstraction.
 *
 * The definition of a "Container" is that of a higher kinded type - it takes a type constructor
 */
class Ex5Suite extends FunSuite with Ex5Fixture {
  test("Empty List Container") {
    assert(Container[List].empty[Int] == Nil)
  }

  test("List Container") {
    assert(Container[List].insert(4, Container[List].insert(5, Nil)) == List(4, 5))
  }
}

trait Ex5Fixture {
  /**
   * Scala is more explicit than Haskell - If a trait is parametrized by a type constructor, it has to be marked as such.
   * The Haskell version is implicit where we see that "c" is applied to another type "a", which means that "c" must be a type constructor:
   * {{{
   *  class Container c where
   *    empty  :: c a
   *    insert :: a -> c a -> c a
   * }}}
   */
  trait Container[C[_]] {
    def empty[A]: C[A]

    def insert[A](x: A, xs: C[A]): C[A]
  }

  object Container {
    def apply[C[_]: Container]: Container[C] =
      implicitly // Or more directly: implicitly[Container[C]]
  }

  implicit val containerList: Container[List] =
    new Container[List] {
      def empty[A]: List[A] =
        Nil

      def insert[A](x: A, xs: List[A]): List[A] =
        x +: xs
    }
}