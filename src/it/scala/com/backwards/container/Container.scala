package com.backwards.container

import java.util.Collections
import org.testcontainers.containers.Network.NetworkImpl
import org.testcontainers.containers.{Network, GenericContainer => JGenericContainer}
import com.backwards.logging.Logging

trait Container {
  val imageName: String

  def start(): Unit

  def stop(): Unit
}

object Container extends Logging {
  def apply(cs: Container*): Container = new Container {
    val imageName: String = "wrapper"

    def start(): Unit = {
      // TODO - Hacky "asInstanceOf" but we are dealing with Java
      lazy val network = Network.newNetwork.asInstanceOf[NetworkImpl]

      cs.foreach {
        case c: JGenericContainer[_] =>
          logger.info(s"Starting networked container ${c.imageName}")
          c.setNetwork(network)
          c.setNetworkAliases(Collections.singletonList(network.getName))
          c.start()

        case c =>
          logger.info(s"Starting container ${c.imageName}")
          c.start()
      }
    }

    def stop(): Unit = cs.reverse.foreach { c =>
      logger.info(s"Stopping container ${c.imageName}")
      c.stop()
    }
  }
}