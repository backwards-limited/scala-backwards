package com.backwards.config

import java.net.{URI, URL}
import java.nio.file.{Path, Paths}
import scala.language.experimental.macros
import scala.reflect.ClassTag
import io.lemonlabs.uri.Uri
import pureconfig._
import pureconfig.generic.ExportMacros
import com.typesafe.config.{ConfigFactory, Config => TypesafeConfig}

object Config extends Config

/**
  * This trait, for convenience, hard codes the use of automatic derivation of readers and writers via copied code from [[pureconfig.generic.auto]]
  */
trait Config {
  type Classpath = String

  implicit def exportReader[A]: Exported[ConfigReader[A]] = macro ExportMacros.exportDerivedReader[A]
  implicit def exportWriter[A]: Exported[ConfigWriter[A]] = macro ExportMacros.exportDerivedWriter[A]

  implicit val uriReader: ConfigReader[Uri] =
    ConfigReader fromString { s =>
      Right(Uri parse s)
    }

  def load[C: ClassTag](namespace: String)(implicit READER: Derivation[ConfigReader[C]]): C =
    loadConfigOrThrow[C](namespace)

  def load[C: ClassTag](classpath: Classpath, namespace: String)(implicit READER: Derivation[ConfigReader[C]]): C =
    loadConfigOrThrow[C](ConfigFactory.load(classpath), namespace)

  // TODO - Not sure I want the rest...

  def load[C: ClassTag](config: TypesafeConfig, namespace: String)(implicit READER: Derivation[ConfigReader[C]]): C =
    loadConfigOrThrow[C](config, namespace)

  def load[C: ClassTag](path: Path, namespace: String)(implicit READER: Derivation[ConfigReader[C]]): C =
    loadConfigOrThrow[C](path, namespace)

  def load[C: ClassTag](url: URL, namespace: String)(implicit READER: Derivation[ConfigReader[C]]): C =
    load[C](url.toURI, namespace)

  def load[C: ClassTag](uri: URI, namespace: String)(implicit READER: Derivation[ConfigReader[C]]): C =
    loadConfigOrThrow[C](Paths get uri, namespace)
}