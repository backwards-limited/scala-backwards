package tech.backwards.bookofmonads.ch3

import cats.implicits.{catsSyntaxOptionId, none}
import munit._

/**
 * Applicatives - ZipList
 */
class Ex3Suite extends FunSuite {
  final case class ZipList[A](value: List[A]) // extends AnyVal

  object ZipList {
    def apply[A](xs: A*): ZipList[A] =
      ZipList(xs.toList)
  }

  test("Applicative for ZipList") {
    import ApplicativeFixture._

    implicit val applicativeZipList: Applicative[ZipList] =
      new Applicative[ZipList] {
        def point[A](x: A): ZipList[A] =
          ZipList(x)

        /**
         * Have to override to unwrap ZipList to get to the inner List and apply the given function.
         */
        override def map[A, B](xs: ZipList[A])(f: A => B): ZipList[B] =
          ZipList(xs.value.map(f))

        def ap[A, B](xs: ZipList[A])(fab: ZipList[A => B]): ZipList[B] = {
          val bs: List[B] =
            xs.value.zip(fab.value).map { case (a, ab) => ab(a) }

          ZipList(bs)
        }
      }


    def comma[A]: A => A => (A, A) =
      a => b => (a, b)

    assertEquals(
      Applicative[ZipList].ap(ZipList(3, 4))(Applicative[ZipList].map(ZipList(1, 2))(comma)),
      ZipList(1 -> 3, 2 -> 4)
    )
  }

  /**
   * A pattern emerges, which will fomulate the "applicative style".
   *
   * Each time we want to use a pure function in a monadic (or applicative) context, we use:
   * fmap f x1 `ap` x2 `ap` ... `ap` xN
   *
   * by introducing symbols we get:
   * f <$> x1 <*> x2 <*> ... <*> xN
   *
   * Now instead of the following do block:
   *
   * do x <- [1,2,3]
   *    y <- [4,5,6]
   *    return (x + y)
   *
   * we can instead, in applicative style:
   * (+) <$> [1,2,3] <*> [4,5,6]
   */
  test("Applicative style") {
    import ApplicativeFixture._
    import FunctorFixture._

    implicit object ApplicativeOption extends Applicative[Option] {
      def point[A](x: A): Option[A] =
        x.some

      def ap[A, B](Fa: Option[A])(Ff: Option[A => B]): Option[B] =
        (Fa, Ff) match {
          case (Some(a), Some(f)) =>
            point(f(a))

          case _ =>
            None
        }
    }

    implicit class FunctionExtension[A, B](f: A => B) {
      def map[F[_]: Functor](Fa: F[A]): F[B] =
        Functor[F].map(Fa)(f)
    }

    implicit class ApplicativeExtension[F[_]: Applicative, A](Fa: F[A]) {
      def ap[B](Ff: F[A => B]): F[B] =
        Applicative[F].ap(Fa)(Ff)
    }

    implicit class ApplicativeFunctionExtension[F[_]: Applicative, A, B](Ff: F[A => B]) {
      def ap(Fa: F[A]): F[B] =
        Applicative[F].ap(Fa)(Ff)
    }

    val f: Int => String => Boolean =
      i => s => true

    assertEquals(
      "1".some.ap(f.map(1.some)),
      true.some
    )

    assertEquals(
      f map 1.some ap "1".some,
      true.some
    )
  }

  /**
   * In the realm of pure computation, it never makes sense to execute code if you do not want its return value, since that is the only visible effect coming out of that call.
   * For monadic or applicative computations, this is not the case, as you might want to execute one of these actions for its side effects alone, completely disregarding the value returned from the action.
   * Haskell’s standard library provides three, special combinators for that case:
   * {{{
   *    (<$) :: Functor     f =>   a -> f b -> f a
   *
   *    (<*) :: Applicative f => f a -> f b -> f a
   *
   *    (*>) :: Applicative f => f a -> f b -> f b
   * }}}
   */
  test("Applicative goodies") {
  }

  object FunctorFixture {
    trait Functor[F[_]] {
      def map[A, B](x: F[A])(f: A => B): F[B]
    }

    object Functor {
      def apply[F[_]: Functor]: Functor[F] = implicitly
    }
  }

  object ApplicativeFixture {
    import FunctorFixture._

    trait Applicative[F[_]] extends Functor[F] {
      def point[A](x: A): F[A]

      def ap[A, B](x: F[A])(f: F[A => B]): F[B]

      def map[A, B](x: F[A])(f: A => B): F[B] =
        ap(x)(point(f))
    }

    object Applicative {
      def apply[F[_]: Applicative]: Applicative[F] = implicitly
    }
  }
}