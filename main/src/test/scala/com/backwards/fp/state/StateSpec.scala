package com.backwards.fp.state

import cats.data.{Kleisli, State}
import org.scalatest.matchers.must.Matchers
import org.scalatest.wordspec.AnyWordSpec

/**
  * [[https://functional.works-hub.com/learn/a-simple-way-to-write-parsers-using-the-state-monad-d5863]]
  */
class StateSpec extends AnyWordSpec with Matchers {
  "blah" should {
    import cats.implicits._

    "blah" in {
      val inc: Int => Int =
        _ + 1

      val times2: Int => Int =
        _ * 2

      val r = inc andThen times2

      r(10)


      val incEff: Int => Option[Int] =
        i => (i + 1).some

      val times2Eff: Int => Option[Int] =
        i => (i * 2).some

      val rEff = Kleisli(incEff) andThen times2Eff

      println(rEff.run(10))
    }
  }
  "State " should {
    /*"be simple" in {
      val c = State[String, Char](s => (s.tail, s.head))

      c.run("re").value   // (String, Char) = (e, r)
      c.run("are").value  // (String, Char) = (re, a)
    }*/

    "be monadic" in {
      val head = State[String, Char] { s =>
        println(s"head: s = $s")
        (s.tail, s.head)
      }

      val toInt = State[String, Int] { s =>
        println(s"toInt: s = $s")
        ("", s.toInt)
      }

      var x = 1

      lazy val product: State[String, Int] = for {
        h <- head
        _ = println(s"h = $h")
        i <- toInt
        _ = println(s"i = $i")
      } yield h * i

      val initialState = "23"

      /*
      "23" => ("3", '2')
      "3" => ("", 3)
      */
      product.run(initialState).value mustBe ("", 150) // '2' * 3 = 50 * 3 = 150
    }
  }
}