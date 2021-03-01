package com.backwards.fp.pancakes

import io.circe.Codec
import io.circe.generic.extras.Configuration
import io.circe.generic.extras.semiauto.deriveConfiguredCodec

trait PancakeIngredientCodecs {
  private implicit val config: Configuration =
    Configuration.default
      .withDiscriminator("ingredient")
      .withSnakeCaseConstructorNames

  implicit val pancakeIngredientCodec: Codec[PancakeIngredient] =
    deriveConfiguredCodec[PancakeIngredient]
}