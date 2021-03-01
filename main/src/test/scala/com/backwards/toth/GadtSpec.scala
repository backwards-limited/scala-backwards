package com.backwards.toth

import org.scalatest.matchers.must.Matchers
import org.scalatest.wordspec.AnyWordSpec

/**
 * [[https://contramap.dev/posts/2019-11-30-gadt/ GADTS in a nutshell]]
 */
class GadtSpec extends AnyWordSpec with Matchers {
  "" should {
    "" in {
      sealed trait Foo[A]

      case class Bar[A](value: A)                       extends Foo[A]

      case class BarInt(value: Int)                     extends Foo[Int]

      case class BarMap[A, B](value: Foo[B], f: B => A) extends Foo[A]


      def run[A](f: Foo[A]): A = f match {
        case Bar(value)        => value   // `a` is of type `A`

        case BarInt(i)         => i       // `i` is an `Int`

        case map: BarMap[A, b] =>
          val f: b => A = map.f
          val v: Foo[b] = map.value
          f(run(v)) // returns an `A`
      }

      val fooInt: Foo[Int] = BarInt(42)

      val x: Int = run(fooInt)
      x mustBe 42


      val foo: Foo[Int] = BarInt(42)

      val mapFoo: Foo[Int] = BarMap(foo, (_: Int) + 1)

      val y: Int = run(mapFoo)
      y mustBe 43
    }
  }
}