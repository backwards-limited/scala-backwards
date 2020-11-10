package com.backwards.fp.typeclass

/**
  * Typeclass (general) pattern
  *
  * @tparam A Type that will have a typeclass instance associated with it
  */
trait TypeClass[A] {
  def foo(a: A): String
}

object TypeClass {
  def foo[A: TypeClass](a: A): String = implicitly[TypeClass[A]] foo a
}

object TypeClassInstances {
  implicit val intInstance: TypeClass[Int] = (_: Int) => "int"

  implicit val stringInstance: TypeClass[String] = (_: String) => "string"
}