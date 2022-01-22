package tech.backwards.catz.functor

import cats.Functor
import org.scalatest.matchers.must.Matchers
import org.scalatest.wordspec.AnyWordSpec

class BlahSpec extends AnyWordSpec with Matchers {
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