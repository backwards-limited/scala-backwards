package com.backwards.container

import org.testcontainers.containers.Network.NetworkImpl
import org.testcontainers.containers.{Network, GenericContainer => JGenericContainer}

class GenericContainer(val imageName: String) extends JGenericContainer[GenericContainer](imageName) with Container {
  def network: Network = getNetwork

  def networkName: String = network match {
    case n: NetworkImpl => n.getName
    case n => n.getId // TODO - Might not be a wise choice
  }
}