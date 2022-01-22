package tech.backwards.container

import org.testcontainers.containers.Network.NetworkImpl
import org.testcontainers.containers.{Network, GenericContainer => JGenericContainer}

/**
  * Starting up a Container for testing is best to be done with dynamic port(s) to avoid port conflicts.
  * However, port binding can be set up if required.
  * @param imageName String The name of the required image to download from an appropriate Docker Registry
  * @tparam SELF Allowing for fluent interface
  */
class GenericContainer[SELF <: JGenericContainer[SELF]](val imageName: String) extends JGenericContainer[SELF](imageName) with Container {
  def network: Network = getNetwork

  def networkName: String = network match {
    case n: NetworkImpl => n.getName
    case n => n.getId // TODO - Might not be a wise choice
  }

  def exposePort(portBinding: (HostPort, ContainerPort)): HostPort = {
    val (hostPort, containerPort) = portBinding
    addFixedExposedPort(hostPort.value, containerPort.value)
    hostPort
  }

  def exposePorts(portBindings: (HostPort, ContainerPort)*): Unit =
    portBindings.foreach(exposePort)
}