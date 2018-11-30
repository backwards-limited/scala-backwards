package com.backwards.fp.typeclass

import scala.reflect.runtime.universe.TypeTag
import shapeless.labelled.FieldType
import shapeless.{::, HList, HNil, LabelledGeneric, Lazy, Witness}

trait Show[A] {
  def apply(a: A): String
}

/**
  * With Scala 2.12 we can use Single Abstract Methods. However, if that were unavailable, or we had more than one parameter, then:
  * {{{
  *   def instance[A](func: A => String): Show[A] = new Show[A] {
  *     def show(a: A): String = func(a)
  *   }
  *
  *   implicit val intShow: Show[Int] =
  *     instance(int => s"int $int")
  *
  *   implicit val stringShow: Show[String] =
  *     instance(str => s"string $str")
  * }}}
  *
  * Note on Shapeless:
  * Shapeless is all about automatically deriving type class instances for our own created types.
  *
  * Note on ShowOps:
  * Original was
  * {{{
  *   implicit class ShowOps[A: Show](a: A) {
  *     def show: String = Show[A](a)
  *   }
  * }}}
  *
  * However, it is for efficient (avoiding a runtime overhead) to declare as a Value Class,
  * where the typeclass constraint needs to be moved to the show function.
  */
object Show {
  def apply[A: Show]: Show[A] = implicitly

  def apply[A: Show](a: A): String = Show[A].apply(a)

  object ops {
    def show[A: Show](a: A): String = Show[A](a)

    implicit class ShowOps[A](val a: A) extends AnyVal {
      def show(implicit sh: Show[A]): String = sh(a)
    }
  }

  implicit val intShow: Show[Int] =
    int => s"int $int"

  implicit val stringShow: Show[String] =
    str => s"string $str"

  implicit def genericShow[A, H <: HList](
    implicit GEN: LabelledGeneric.Aux[A, H],
    SHOW: Lazy[Show[H]],
    TAG: TypeTag[A]
  ): Show[A] = { a =>
    val sh = SHOW value GEN.to(a)
    s"${TAG.tpe} :: $sh"
  }

  implicit val hnilShow: Show[HNil] =
    hnil => ""

  implicit def hlistShow[K <: Symbol, H, T <: HList](
    implicit WITNESS: Witness.Aux[K],
    HSHOW: Show[H],
    TSHOW: Show[T]
  ): Show[FieldType[K, H] :: T] = { case h :: t =>
    val name = WITNESS.value.name
    val head = HSHOW(h)
    val tail = TSHOW(t)

    if (tail.isEmpty) s"$name: $head" else s"$name: $head, $tail"
  }
}