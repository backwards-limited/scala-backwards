package com.backwards

import java.io.FileInputStream
import java.util.Properties
import scala.collection.JavaConverters._
import com.backwards.logging.Logging

/**
  * There is a nice [SBT plugin](https://github.com/mefellows/sbt-dotenv) that essentially does the same thing.
  * However, it would seem the plugin would always pick up and apply configuration in a ".env" and sometimes I don't want that.
  */
trait AppBackwards extends App with Logging {
  Option(System getProperty "ENV") foreach { env =>
    info("Loading given environment into JVM's system properties...")

    val properties = new Properties
    properties load new FileInputStream(env)

    propertiesAsScalaMap(properties) foreach { case (key, value) =>
      info(s"Setting system property: $key...")
      System.setProperty(key, value)
    }
  }
}