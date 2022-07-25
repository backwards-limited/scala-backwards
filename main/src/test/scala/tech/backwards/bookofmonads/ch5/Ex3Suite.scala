package tech.backwards.bookofmonads.ch5

import cats.implicits.{catsSyntaxFlatMapOps, catsSyntaxOptionId, none}
import cats.{Applicative, Monad}
import munit.FunSuite

/**
 * Monad Laws - What we call laws, mathematicians call theorems... and Kleisli.
 *
 * In these tests we simplify polymorphic monadic functions (f and g).
 * Ideally we would have something like the following, but usage would make the tests less readable:
 * {{{
 *   def f[F[_]: Applicative, A, B]: A => F[B]
 * }}}
 */
class Ex3Suite extends FunSuite {
  test("Left identity") {
    val a = 1

    def unit[A](a: A): Option[A] =
      Monad[Option].pure(a)

    def f[A]: A => Option[A] =
      Applicative[Option] pure

    assertEquals(
      unit(a) >>= f,
      f(a)
    )
  }

  test("Right identity") {
    val a = 1

    def unit[A](a: A): Option[A] =
      Monad[Option].pure(a)

    def f[A]: A => Option[A] =
      Applicative[Option] pure

    assertEquals(
      f(a) >>= unit,
      f(a)
    )
  }

  test("Associativity") {
    val a = 1

    def unit[A](a: A): Option[A] =
      Monad[Option].pure(a)

    def f[A]: A => Option[A] =
      Applicative[Option] pure

    def g[A]: A => Option[A] =
      Applicative[Option] pure

    assertEquals(
      unit(a) >>= f >>= g,
      unit(a) >>= (x => f(x) >>= g)
    )
  }

  /**
   * {{{
   *  a -> m b, is called a Kleisli arrow for the monad m.
   *  e.g.
   *  validateName :: String -> Maybe Name, is a Kleisli arrow for the Maybe monad.
   *
   *  Remember the composition of two pure functions is:
   *  (.) :: (b -> c) -> (a -> b) -> a -> c
   *
   *  whereas, the version for Kleisli arrows is:
   *  (<=<) :: Monad m => (b -> m c) -> (a -> m b) -> a -> m c
   *  f <=< g = \x -> do y <- g x
   *                     f y
   *
   *  OR (maybe nicer):
   *  f <=< g = \x -> g x >>= f
   * }}}
   */
  test("Kleisli composition") {
    def composeK[F[_]: Monad, A, B, C](f: B => F[C])(g: A => F[B])(a: A): F[C] =
      g(a) >>= f

    val g: String => Option[Int] =
      s => s.toIntOption

    val f: Int => Option[Boolean] =
      i => if (i % 2 == 0) true.some else none

    assert(composeK(f)(g)("4").isDefined)

    assert(composeK(f)(g)("5").isEmpty)
  }

  /**
   * We cannot just use the usual identity of "a -> a", since that is not the shape of Kleisli.
   * Instead we need to use identity "a -> m a", which is equivalent to to Monad's "pure", "point", and "return" in Haskell. Giving us:
   * {{{
   *  return <=< f eq f                     -- Left identity
   *
   *  f <=< return eq f                     -- Right identity
   *
   *  (f <=< g) <=< h eq f <=< (g <=< h)    -- Associativity
   * }}}
   *
   * We can now think of monad laws as nothing but the usual function laws with functions replaced by Kleisli arrows.
   */
  test("Use Kleisli composition to simplify Monad laws") {
    def composeK[F[_]: Monad, A, B, C](f: B => F[C])(g: A => F[B])(a: A): F[C] =
      g(a) >>= f

    val g: String => Option[Int] =
      s => s.toIntOption

    val f: Int => Option[Boolean] =
      i => if (i % 2 == 0) true.some else none

    def `return`[A]: A => Option[A] =
      Applicative[Option].pure[A]

    assertEquals(
      composeK[Option, String, Int, Int](`return`)(g)("0"),
      g("0")
    )

    assertEquals(
      composeK[Option, String, String, Int](g)(`return`[String])("0"),
      g("0")
    )
  }
}