package tech.backwards.config

import io.lemonlabs.uri.Uri

final case class BootstrapConfig(servers: Seq[Uri]) {
  lazy val bootstrapServers: String =
    servers.map(_.toStringRaw).mkString(",")
}