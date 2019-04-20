package com.backwards.config

import scala.language.experimental.macros
import scala.reflect.ClassTag
import io.lemonlabs.uri.Uri
import pureconfig._
import pureconfig.generic.ExportMacros

/**
  * This trait, for convenience, hard codes the use of automatic derivation of readers and writers via copied code from [[pureconfig.generic.auto]]
  */
trait Config {
  implicit def exportReader[A]: Exported[ConfigReader[A]] = macro ExportMacros.exportDerivedReader[A]
  implicit def exportWriter[A]: Exported[ConfigWriter[A]] = macro ExportMacros.exportDerivedWriter[A]

  implicit val uriReader: ConfigReader[Uri] =
    ConfigReader fromString { s =>
      Right(Uri parse s)
    }

  def load[C: ClassTag](namespace: String)(implicit READER: Derivation[ConfigReader[C]]): C =
    loadConfigOrThrow[C](namespace)
}

object Config extends Config