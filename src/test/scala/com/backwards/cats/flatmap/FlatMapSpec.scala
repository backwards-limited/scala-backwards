package com.backwards.cats.flatmap

import cats.FlatMap
import mouse.boolean._
import org.scalatest.{MustMatchers, WordSpec}

/**
  * Adding to an Apply the methods flatten and flatMap, we are rewarded with a FlatMap and one step away from a Monad.
  *
  * Note the use of Mouse, a Cats companion, with convenient extensions such as:
  * - given a boolean condition, `.option` will return None if false,
  * - otherwise it will return an Option of the argument
  * e.g.
  * {{{
  *   ("Hello".length == 1).option("World") // None
  *   ("Hello".length != 0).option("World") // Some("World")
  * }}}
  */
class FlatMapSpec extends WordSpec with MustMatchers {
  "FlatMap" should {
    "opton" in {
      import cats.instances.option._
      import cats.syntax.option._

      FlatMap[Option].flatten(1.some.some) mustBe Option(1)

      val fOpt = (x: Int) => (x == 1).option(true)

      FlatMap[Option].flatMap(none)(fOpt) mustBe None
      FlatMap[Option].flatMap(1.some)(fOpt) mustBe Some(true)
    }

    "list" in {
      import cats.instances.list._

      FlatMap[List].flatten(List(List(1))) mustBe List(1)

      val fStr = (x: String) => x.toCharArray.toList

      FlatMap[List].flatMap(List.empty)(fStr) mustBe List()
      FlatMap[List].flatMap(List("Hello!"))(fStr) mustBe List('H', 'e', 'l', 'l', 'o', '!')
    }
  }
}