package com.backwards.typelevel.learning1

import shapeless.the

/**
 * Difference between implicitly and the and dependent type reserving
 */
object L11 extends App {

  trait Foo {
    type T

    val t: T
  }

  implicit val intFoo: Foo { type T = Int } =
    new Foo {
      type T = Int

      val t = 42
    }

  // Doesn't work because `implicitly` doesn't preserve dependent types
  //  val int: Int = implicitly[Foo].t + 1

  val int: Int = the[Foo].t + 1
}