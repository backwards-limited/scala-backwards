package com.backwards.config

import scala.reflect.ClassTag
import io.lemonlabs.uri.Uri
import pureconfig._

trait Config {
  implicit val uriReader: ConfigReader[Uri] =
    ConfigReader fromString { s =>
      Right(Uri parse s)
    }

  def load[C: ClassTag](namespace: String)(implicit reader: Derivation[ConfigReader[C]]): C =
    loadConfigOrThrow[C](namespace)
}

object Config extends Config