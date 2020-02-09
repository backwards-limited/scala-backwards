package com.backwards

import java.io.FileInputStream
import java.util.Properties
import scala.jdk.CollectionConverters._

/**
  * There is a nice [SBT plugin](https://github.com/mefellows/sbt-dotenv) that essentially does the same thing.
  * However, it would seem the plugin would always pick up and apply configuration in a ".env" and sometimes I don't want that.
  */
trait AppBackwards extends App {
  Option(System getProperty "ENV") foreach { env =>
    scribe info "Loading given environment into JVM's system properties..."

    val properties = new Properties
    properties load new FileInputStream(env)

    properties.asScala foreach { case (key, value) =>
      scribe info s"Setting system property: $key..."
      System.setProperty(key, value)
    }
  }
}