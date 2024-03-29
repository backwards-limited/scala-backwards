package tech.backwards.fp.mtl

import cats.data.Writer
import cats.{Applicative, Functor, Monad}
import cats.mtl.{Raise, Tell}
import org.scalatest.matchers.must.Matchers
import org.scalatest.wordspec.AnyWordSpec

class MTLExamplesSpec extends AnyWordSpec with Matchers {
  "FunctorRaise" should {
    class Example[M[_]: Applicative: Raise[*[_], String]] {
      def twoDivideBy(den: Double): M[Double] =
        if (den == 0) Raise[M, String].raise("Cannot divide by zero")
        else Applicative[M].pure(2 / den)
    }

    "run with Either" in {
      import cats.mtl.implicits._

      val ex = new Example[String Either *]

      ex.twoDivideBy(2) mustBe Right(1)
      ex.twoDivideBy(0) mustBe Left("Cannot divide by zero")
    }

    "run with custom type class instance" in {
      sealed abstract class Maybe[+A]

      final case class Just[+A](value: A) extends Maybe[A]

      final case object Nothing extends Maybe[Nothing]

      implicit val maybeFunctor: Functor[Maybe] = new Functor[Maybe] {
        def map[A, B](fa: Maybe[A])(f: A => B): Maybe[B] = ???
      }

      implicit val maybeApplicative: Applicative[Maybe] = new Applicative[Maybe] {
        def pure[A](a: A): Maybe[A] = Just(a)

        def ap[A, B](ff: Maybe[A => B])(fa: Maybe[A]): Maybe[B] = ???
      }

      implicit def maybeFunctorRaise[F[_]: Functor]: Raise[Maybe, String] =
        new Raise[Maybe, String] {
          def functor: Functor[Maybe] = implicitly

          def raise[E2 <: String, A](e: E2): Maybe[A] = Nothing
        }

      val ex = new Example[Maybe]

      ex.twoDivideBy(2) mustBe Just(1)
      ex.twoDivideBy(0) mustBe Nothing
    }
  }

  "FunctorTell" should {
    class Example[M[_]: Monad: Tell[*[_], Vector[String]]] {
      import cats.syntax.all._

      def apply(): M[Unit] = {
        for {
          value <- Monad[M].pure("My value")
          _ <- Tell[M, Vector[String]].tell(Vector("Value found"))
          _ <- Tell[M, Vector[String]].tell(Vector(value))
        } yield ()
      }
    }

    "run with Writer" in {
      import cats.mtl.implicits._

      val ex = new Example[Writer[Vector[String], *]]

      val v: (Vector[String], Unit) = ex().run

      println(v)
    }

    "run with custom type class instance" in {
      // TODO
    }
  }
}