package tech.backwards.dependenttypes

import org.scalatest.matchers.must.Matchers
import org.scalatest.wordspec.AnyWordSpec

class TypeClassSpec extends AnyWordSpec with Matchers {
  "Type class" should {
    "have instances" in {
      trait Compares[A] {
        def cmp(a1: A, a2: A): Boolean
      }

      object Compares {
        def apply[A: Compares]: Compares[A] = implicitly

        object syntax {
          // Syntax introduces extensions methods - Simulacrum can replace this
          implicit class ComparesOps[A: Compares](a: A) {
            def cmp(aOther: A): Boolean =
              Compares[A].cmp(a, aOther)
          }
        }
      }

      implicit val intCompares: Compares[Int] =
        _ < _

      implicit def listCompares[A: Compares]: Compares[List[A]] =
        (a1: List[A], a2: List[A]) => a1.zip(a2).forall(Compares[A].cmp _ tupled)

      println(Compares[Int].cmp(10, 20))

      println(Compares[List[Int]].cmp(List(10, 20), List(100, 200)))

      import Compares.syntax._

      println(10 cmp 20)

      println(List(10, 20) cmp List(100, 200))
    }
  }

  // TODO - Does not compile
  /*
  "Simulacrum type class" should {
    "have instances" in {
      // In Simulacrum, think of the @op as the extension method e.g. @("cmp") defines extension method cmp for the type class function cmp
      @typeclass
      trait Compares[A] {
        @op("cmp") def cmp(a1: A, a2: A): Boolean
      }

      implicit val intCompares: Compares[Int] =
        _ < _

      implicit def listCompares[A: Compares]: Compares[List[A]] =
        (a1: List[A], a2: List[A]) => a1.zip(a2).forall(Compares[A].cmp _ tupled)

      println(Compares[Int].cmp(10, 20))

      println(Compares[List[Int]].cmp(List(10, 20), List(100, 200)))

      import Compares.ops._

      println(10 cmp 20)

      println(List(10, 20) cmp List(100, 200))
    }
  }
  */
}