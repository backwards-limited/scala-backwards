package com.backwards.cats.functor

import scala.language.higherKinds
import cats.Functor
import org.scalatest.{MustMatchers, WordSpec}

class BlahSpec extends WordSpec with MustMatchers {
  "" should {
    "" in {
      import SomeImplicits._

      println(blah(Blah("1")))
    }
  }

  def blah[T[_]: Functor](t: T[String]): T[String] = {
    implicitly[Functor[T]].map(t)(a => "hi")
  }
}

case class Blah[A](a: A)

object SomeImplicits {
  implicit val blahFunctor: Functor[Blah] = new Functor[Blah] {
    override def map[A, B](fa: Blah[A])(f: A => B): Blah[B] = Blah(f(fa.a))
  }
}