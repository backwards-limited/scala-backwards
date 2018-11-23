package com.backwards.config

import scala.reflect.ClassTag
import io.lemonlabs.uri.Uri
import pureconfig.{ConfigReader, Derivation, loadConfigOrThrow}

@deprecated(message = "Instead use com.backwards.config.Config", since = "18th November 2018")
trait ConfigOps {
  implicit val uriReader: ConfigReader[Uri] =
    ConfigReader fromString { s =>
      Right(Uri parse s)
    }

  def load[C: ClassTag](namespace: String)(implicit reader: Derivation[ConfigReader[C]]): C =
    loadConfigOrThrow[C](namespace)
}