package com.backwards.fp.functor

import java.util.Date
import org.scalatest.matchers.must.Matchers
import org.scalatest.wordspec.AnyWordSpec

/**
 * [[https://blog.softwaremill.com/scala-cats-invariant-functor-be57d2e2fa91 Cats Invariant Functor]]
 */
class InvariantFunctorSpec extends AnyWordSpec with Matchers {
  "Invariant Functor" should {
    "demo" in {
      /*
      Functor (or Covariant Functor):

      trait Functor[F[_]] {
        def map[A, B](fa: F[A])(f: A => B): F[B]
      }

      Contravariant Functor has types for f switched:

      def contramap[A, B](fa: F[A])(f: B => A): F[B]

      which gets useful when we want to provide new implicit implementations of some type class by reusing implementations already available.

      In short, Functor’s map is used when we append operations in a chain and Contravariant Functor’s contramap is used when we want to prepend them.

      There is a third option that allows us to go in both directions with imap and this is specific to the Invariant Functor type.

      The most common and easiest example of Invariant Functors include codecs and parsers, where we need to have the ability to transform something in both directions.
      */

      trait CustomParser[A] { self =>
        def encode(value: A): String

        def decode(value: String): A

        def imap[B](dec: A => B, enc: B => A): CustomParser[B] =
          new CustomParser[B] {
            def encode(value: B): String =
              self.encode(enc(value))

            def decode(value: String): B =
              dec(self.decode(value))
          }
      }

      def encode[A: CustomParser](value: A): String =
        implicitly[CustomParser[A]].encode(value)

      def decode[A: CustomParser](value: String): A =
        implicitly[CustomParser[A]].decode(value)

      /*
      Let’s say we have an implementation of our CustomParser for Long values in our implicit scope and we want to get another parser but for java.util.Date type.
      We know how to convert Date to and from a Long value, therefore using the Invariant Functor idea, we can provide a new parser very easily:
      */

      implicit val longParser: CustomParser[Long] =
        new CustomParser[Long] {
          def encode(value: Long): String = value.toString

          def decode(value: String): Long = value.toLong
        }

      implicit val dateParser: CustomParser[Date] =
        longParser.imap(new Date(_), _.getTime)
    }
  }

  "Cats Invariant Functor" should {
    "demo" in {
      /*
      Having a Semigroup[Long] already provided by Cats, we can easily add new Semigroup instances for new types when we know how to convert from one type to another and back.
      The example is using Long -> Date and Date -> Long transformations (again):
      */

      import cats._
      import cats.implicits._

      val longToDate: Long => Date =
        new Date(_)

      val dateToLong: Date => Long =
        _.getTime

      implicit val semigroupDate: Semigroup[Date] =
        Semigroup[Long].imap(longToDate)(dateToLong)

      val today: Date =
        longToDate(1449088684104L)

      val timeLeft: Date =
        longToDate(1900918893L)

      today |+| timeLeft
      // res1: Date = Thu Dec 24 21:40:02 CET 2015
    }
  }
}