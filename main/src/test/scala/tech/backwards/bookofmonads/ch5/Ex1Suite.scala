package tech.backwards.bookofmonads.ch5

import cats.implicits._
import cats.{Functor, Semigroup}
import munit.FunSuite

/**
 * Monad Laws - What we call laws, mathematicians call theorems.
 * First, Function laws; Functor laws; and Monoids
 */
class Ex1Suite extends FunSuite with FunctionFixture { self =>
  test("Function left identity law") {
    val id: String => String =
      self.id[String]

    val f: Int => String =
      _.toString

    assertEquals(
      (id `.` f)(20),
      f(20)
    )
  }

  test("Function right identity law") {
    val id: Int => Int =
      self.id[Int]

    val f: Int => String =
      _.toString

    assertEquals(
      (f `.` id)(20),
      f(20)
    )
  }

  test("Function associativity") {
    type A = Int
    type B = Int
    type C = Int
    type D = Int

    val f: C => D =
      self.id[D]

    val g: B => C =
      self.id[C]

    val h: A => B =
      self.id[B]

    assertEquals(
      (f `.` (g `.` h))(1),
      ((f `.` g) `.` h)(1)
    )
  }

  test("Function composition is NOT commutative") {
    // Addition is commutative e.g.
    assertEquals(1 + 3, 3 + 1)

    // But
    // f . g DOES NOT EQUATE TO g . f
  }

  /*
  There are many ways to prove that a law holds.
  The simplest technique, called equational reasoning, works by substituting equal expressions for equal expressions in tiny steps, until we obtain the equality we want.
  We follow a style in which every eq sign includes the reason for the equality between the two expressions surrounding it.
  For example, the proof of function associativity looks like this:

    (f . (g . h)) x
  eq -- by definition of (.)
    f ((g . h) x)
  eq -- by definition of (.)
    f (g (h x))
  eq -- by definition of (.)
    (f . g) (h x)
  eq -- by definition of (.)
    ((f . g) . h) x
  */

  test("Functor law: fmap id `eq` id") {
    assertEquals(
      fmap[Option, Int, Int](identity[Int] _).apply(1.some),
      identity(1.some)
    )
  }

  test("Functor law: fmap (f . g) `eq` fmap f . fmap g") {
    val f: Boolean => String =
      _.toString

    val g: Int => Boolean =
      _ == 0

    assertEquals(
      fmap[Option, Int, String](f `.` g).apply(1.some),
      (fmap[Option, Boolean, String](f) `.` fmap[Option, Int, Boolean](g))(1.some)
    )

    assertEquals(
      fmap(1.some)(f `.` g),
      (fmap[Option, Boolean, String](f) `.` fmap[Option, Int, Boolean](g))(1.some)
    )
  }
  /*
  Another way to state these laws is that a functor must preserve the structure of the container it maps over untouched.
  It should only be able to affect whatever information is encoded inside of that container.
  As "id" returns its input as it is, the fact that mapping "id" over any structure keeps the whole structure the same.
  Since "fmap id" equals "id" — tells us that "fmap" itself only has the power to modify the elements inside a container and not the container itself.
  */

  /*
  The functor laws guarantee that only elements are affected by a functorial operation.
  If we want the first law to hold, every constructor in the data type must be mapped to the exact same constructor by fmap.
  The second law tells us that if we apply two functions in a row, it does not matter whether we apply both at once over the elements or we traverse the structure twice.
  */

  /*
  Monoid:

  class Monoid m where
    mempty  :: m
    mappend :: m -> m -> m

  trait Monoid[A] {
    def empty: A
    def combine(x: A, y: A): A
  }
  */
  test("Monoid left and right identity: e combine x = x = x combine e (where e is empty)") {
  }

  test("Monoid associativity: (x combine y) combine z = x combine (y combine z)") {
  }

  test("Left biased Option Monoid") {
    def semigroupOptionLeft[A]: Semigroup[Option[A]] =
      (x: Option[A], y: Option[A]) => if (x.isEmpty) y else x

    assertEquals(
      semigroupOptionLeft[Int].combine(None, 1.some),
      1.some
    )

    assertEquals(
      semigroupOptionLeft[Int].combine(2.some, 1.some),
      2.some
    )
  }

  test("Right biased Option Monoid") {
    def semigroupOptionRight[A]: Semigroup[Option[A]] =
      (x: Option[A], y: Option[A]) => if (y.isEmpty) x else y

    assertEquals(
      semigroupOptionRight[Int].combine(None, 1.some),
      1.some
    )

    assertEquals(
      semigroupOptionRight[Int].combine(2.some, 1.some),
      1.some
    )
  }
}

trait FunctionFixture { self =>
  def fmap[F[_]: Functor, A, B](f: A => B): F[A] => F[B] =
    _.map(f)

  def fmap[F[_]: Functor, A, B](Fa: F[A])(f: A => B): F[B] =
    Fa.map(f)

  // Instead of using function def identity[A](x: A): A = x, we'll define our own:
  def id[A](a: A): A = a

  // Compose - Haskell uses "." so we'll do the same... kind of:
  def `.`[A, B, C]: (B => C) => (A => B) => A => C =
    f => g => a => f(g(a))

  implicit class FunctionSyntax[B, C](f: B => C) {
    def `.`[A](g: A => B): A => C =
      (self `.` f)(g)
  }
}