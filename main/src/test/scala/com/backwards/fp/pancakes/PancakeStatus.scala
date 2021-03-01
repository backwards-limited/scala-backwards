package com.backwards.fp.pancakes

sealed trait PancakeStatus

final case class IngredientReceived(ingredient: PancakeIngredient) extends PancakeStatus

case object PancakeReady extends PancakeStatus