package com.backwards.fs2

import scala.jdk.CollectionConverters._
import cats.implicits._
import monix.eval.Task
import monix.execution.Scheduler
import monix.execution.schedulers.SchedulerService
import monix.kafka.{KafkaConsumerConfig, KafkaConsumerObservable, KafkaProducer, KafkaProducerConfig}
import monocle.Lens
import monocle.macros.GenLens
import org.apache.kafka.clients.consumer.KafkaConsumer
import org.scalatest.Suite
import org.scalatest.matchers.must.Matchers
import org.scalatest.wordspec.AnyWordSpec
import org.testcontainers.lifecycle.Startable
import com.dimafeng.testcontainers.ContainerDef
import com.dimafeng.testcontainers.lifecycle.Stoppable
//import com.dimafeng.testcontainers.KafkaContainer
import com.dimafeng.testcontainers.scalatest.{IllegalWithContainersCall, TestContainerForAll}
import fs2._
import org.scalatest.SuiteMixin

trait KafkaContainerForAll extends TestContainerForAll {
  this: Suite =>

  import com.dimafeng.testcontainers.KafkaContainer

  override val containerDef: KafkaContainer.Def = KafkaContainer.Def()

  def withKafkaContainer(runTest: Containers => Unit): Unit = {
    withContainers { kafkaContainer =>
      println(kafkaContainer.mappedPort(9092))
      runTest(kafkaContainer)
    }
  }
}

/**
 * kafkacat -P -b localhost:9092 -t my-topic
 *
 * kafkacat -C -b localhost:9092 -t my-topic -o beginning
 */
// TODO - Issues after upgrading libraries
class FS2KafkaSpec extends AnyWordSpec with Matchers with KafkaContainerForAll {
  implicit val io: SchedulerService = Scheduler.io("monix-kafka-tests")

  "Kafka" should {
    /*"work without FS2" in withContainers { kafkaContainer =>
      val topic = "my-topic"

      val producer: KafkaProducer[String, String] = {
        val bootstrapServers: Lens[KafkaProducerConfig, List[String]] = GenLens[KafkaProducerConfig](_.bootstrapServers)
        val clientId: Lens[KafkaProducerConfig, String] = GenLens[KafkaProducerConfig](_.clientId)
        val maxInFlightRequestsPerConnection: Lens[KafkaProducerConfig, Int] = GenLens[KafkaProducerConfig](_.maxInFlightRequestsPerConnection)

        val kafkaProducerConfig: KafkaProducerConfig => KafkaProducerConfig =
          bootstrapServers.set(List(kafkaContainer.bootstrapServers)) andThen clientId.set("my-producer") andThen maxInFlightRequestsPerConnection.set(1)

        KafkaProducer[String, String](kafkaProducerConfig(KafkaProducerConfig.default), io)
      }

      val consumer: Task[KafkaConsumer[String, String]] = {
        val bootstrapServers: Lens[KafkaConsumerConfig, List[String]] = GenLens[KafkaConsumerConfig](_.bootstrapServers)
        val groupId: Lens[KafkaConsumerConfig, String] = GenLens[KafkaConsumerConfig](_.groupId)
        val enableAutoCommit: Lens[KafkaConsumerConfig, Boolean] = GenLens[KafkaConsumerConfig](_.enableAutoCommit)

        val kafkaConsumerConfig: KafkaConsumerConfig => KafkaConsumerConfig =
          bootstrapServers.set(List(kafkaContainer.bootstrapServers)) andThen groupId.set("kafka-tests") andThen enableAutoCommit.set(true)

        KafkaConsumerObservable.createConsumer[String, String](kafkaConsumerConfig(KafkaConsumerConfig.default), List(topic)).map { consumer =>
          import consumer._

          poll(0)
          seekToBeginning(assignment)
          consumer
        }
      }

      val messages = for {
        _ <- producer.send(topic, "blah blah")
        messages <- consumer.map(_.poll(3000)).map(_.iterator.asScala.toVector.map(_.value))
      } yield messages

      println(messages.runSyncUnsafe())
    }*/

    /*"with FS2 once" in withKafkaContainer { kafkaContainer =>
      val topic = "my-topic"

      val producer: KafkaProducer[String, String] = {
        val bootstrapServers: Lens[KafkaProducerConfig, List[String]] = GenLens[KafkaProducerConfig](_.bootstrapServers)
        val clientId: Lens[KafkaProducerConfig, String] = GenLens[KafkaProducerConfig](_.clientId)
        val maxInFlightRequestsPerConnection: Lens[KafkaProducerConfig, Int] = GenLens[KafkaProducerConfig](_.maxInFlightRequestsPerConnection)

        val kafkaProducerConfig: KafkaProducerConfig => KafkaProducerConfig =
          bootstrapServers.set(List(kafkaContainer.bootstrapServers)) andThen clientId.set("my-producer") andThen maxInFlightRequestsPerConnection.set(1)

        KafkaProducer[String, String](kafkaProducerConfig(KafkaProducerConfig.default), io)
      }

      val s = Stream.eval(producer.send(topic, "blah blah"))

      // TODO - Fails to compile after upgrading to Cats Effect 3
      // println(s.compile.lastOrError.runSyncUnsafe())
      println("hi")
    }*/

    /*"with FS2 repeat" in withContainers { kafkaContainer =>

    }*/
  }
}