package com.backwards.typelevel.learning1

import shapeless._

/**
 * Example of HList declaration
 */
object L2 extends App {
  val product: String :: Int :: Boolean :: HNil =
    "hello" :: 42 :: true :: HNil

  println(product)

  val first: String = product.head
  val second: Int = product.tail.head
  val rest: Boolean :: HNil = product.tail.tail

  // Error
  // val error = product.tail.tail.tail.head

  val newProduct: Long :: String :: Int :: Boolean :: HNil =
    42L :: product

  println(newProduct)
}