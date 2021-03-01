package com.backwards.fp.pancakes

sealed trait PancakeIngredient

final case class Flour(grams: Int) extends PancakeIngredient

final case class Milk(liters: Double) extends PancakeIngredient

final case class Eggs(count: Int) extends PancakeIngredient