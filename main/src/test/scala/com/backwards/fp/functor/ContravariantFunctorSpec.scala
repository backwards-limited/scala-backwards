package com.backwards.fp.functor

import scala.util.Try
import cats.implicits.none
import cats.{Contravariant, Functor, Monoid, Show}
import org.scalatest.matchers.must.Matchers
import org.scalatest.wordspec.AnyWordSpec

/**
 * Functor is all about "map":
 * {{{
 *   def map[A, B](fa: F[A])(f: A => B): F[B]
 * }}}
 *
 * There are times we want to reverse types in the Functor function f,
 * so it will take type B and return type A,
 * but the function itself will still return wrapper of type B
 */
class ContravariantFunctorSpec extends AnyWordSpec with Matchers {
  "Functor" should {
    "actually be a Covarant Functor" in {
      def reverseStringOption(opt: Option[String]): Option[String] =
        opt.map(_.reverse)

      def reverseStringList(lst: List[String]): List[String] =
        lst.map(_.reverse)

      def reverseStringTry(t: Try[String]): Try[String] =
        t.map(_.reverse)

      // Generalized version
      def reverse[F[_]: Functor](container: F[String]): F[String] =
        Functor[F].map(container)(_.reverse)

      reverse(Option("hi")) mustBe Option("ih")
    }
  }

  "Contravariant Functor" should {
    "demo" in {
      /*
      Imagine a simple type class responsible for the transformation of some type T into a Boolean — popular filtering:
      */
      trait Filter[T] {
        def filter(value: T): Boolean
      }

      object Filter {
        def apply[A: Filter]: Filter[A] =
          implicitly[Filter[A]]
      }

      implicit object StringFilter extends Filter[String] {
        override def filter(value: String): Boolean = value.length > 5
      }

      def filter[A: Filter](a: A): Boolean =
        Filter[A].filter(a)

      println(filter("hello"))
      println(filter("hello world!"))

      /*
      imagine that you need a Functor functionality for our Filter type class to convert it from Filter[String] to Filter[Int] using map:
      */
      val filterFunctor: Functor[Filter] =
        new Functor[Filter] {
          override def map[A, B](fa: Filter[A])(f: A => B): Filter[B] =
            (value: B) => ??? // fa filter f(value) <--- We want to to this but cannot... types are the wrong way round
        }

      implicit val filterContravariant: Contravariant[Filter] =
        new Contravariant[Filter] {
          override def contramap[A, B](fa: Filter[A])(f: B => A): Filter[B] =
            (value: B) => fa filter f(value)
        }

      // Let's do some composition:

      val filterString = Filter[String]

      // A composed filter
      implicit val filterInt: Filter[Int] =
        Contravariant[Filter].contramap(filterString)(_.toString)

      println(filter(3))


      // And any type wrapped in Option:
      implicit def filterOption[T: Filter: Monoid]: Filter[Option[T]] =
        filterContravariant.contramap(Filter[T])(_ getOrElse Monoid[T].empty)

      import cats.instances.string._

      println(filter(Option("some string")))
    }

    "show" in {
      val showInts = Show[Int]

      implicit val showOption: Show[Option[Int]] =
        Contravariant[Show].contramap(showInts)(_ getOrElse 0)

      import cats.syntax.show._

      val x = Option(234)
      println(x.show)

      println(none[Int].show)
    }

    "show - shorter version" in {
      val showInts = Show[Int]

      import cats.syntax.contravariant._

      implicit val showOption: Show[Option[Int]] =
        showInts.contramap(_ getOrElse 0)

      import cats.syntax.show._

      val x = Option(234)
      println(x.show)
    }
  }
}