package tech.backwards.fp.typeclass.simulacrum

import simulacrum.typeclass

@typeclass trait Show[A] {
  def show(a: A): String
}