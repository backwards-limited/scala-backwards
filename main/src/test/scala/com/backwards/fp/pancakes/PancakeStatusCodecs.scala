package com.backwards.fp.pancakes

import io.circe.Codec
import io.circe.generic.extras.Configuration
import io.circe.generic.extras.semiauto.deriveConfiguredCodec

trait PancakeStatusCodecs extends PancakeIngredientCodecs {
  private implicit val config: Configuration =
    Configuration.default
      .withDiscriminator("status")
      .withSnakeCaseConstructorNames

  implicit val pancakeStatusCodec: Codec[PancakeStatus] =
    deriveConfiguredCodec[PancakeStatus]
}