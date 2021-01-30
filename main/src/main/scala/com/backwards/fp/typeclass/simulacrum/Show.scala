package com.backwards.fp.typeclass.simulacrum

import simulacrum._

@typeclass trait Show[A] {
  def show(a: A): String
}

/*
object Show {
  implicit val stringShow: Show[String] =
    str => s"simulacrum string $str"
}*/
