package com.backwards.config

final case class MyConfig(bootstrap: BootstrapConfig, properties: Map[String, String] = Map.empty[String, String])