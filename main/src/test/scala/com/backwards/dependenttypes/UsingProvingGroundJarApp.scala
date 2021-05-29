package com.backwards.dependenttypes

import provingground.HoTT._

object UsingProvingGroundJarApp extends App {
  val A = "A" :: Type

  val a = "a" :: A

  println(A)
  println(a)
  println(a !: A)
  println(a.typ)
}