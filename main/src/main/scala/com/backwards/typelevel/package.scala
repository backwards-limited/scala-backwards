package com.backwards

import shapeless.Generic

package object typelevel {
  implicit class GenericOps[A, R](a: A) {
    def toRepr(implicit gen: Generic.Aux[A, R]): R =
      gen to a
  }
}