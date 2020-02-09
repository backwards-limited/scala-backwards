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
import org.scalatest.matchers.must.Matchers
import org.scalatest.wordspec.AnyWordSpec
import com.dimafeng.testcontainers.KafkaContainer
import com.dimafeng.testcontainers.scalatest.TestContainerForAll

/**
 * kafkacat -P -b localhost:9092 -t my-topic
 *
 * kafkacat -C -b localhost:9092 -t my-topic -o beginning
 */
class FS2KafkaSpec extends AnyWordSpec with Matchers with TestContainerForAll {
  override val containerDef: KafkaContainer.Def = KafkaContainer.Def()

  implicit val io: SchedulerService = Scheduler.io("monix-kafka-tests")

  "" should {
    "" in withContainers { kafkaContainer =>

      /////////////////////////////////////////////////

      val bootstrapServersLens: Lens[KafkaProducerConfig, List[String]] = GenLens[KafkaProducerConfig](_.bootstrapServers)
      val clientIdLens: Lens[KafkaProducerConfig, String] = GenLens[KafkaProducerConfig](_.clientId)
      val maxInFlightRequestsPerConnectionLens: Lens[KafkaProducerConfig, Int] = GenLens[KafkaProducerConfig](_.maxInFlightRequestsPerConnection)

      val blah: KafkaProducerConfig => KafkaProducerConfig = bootstrapServersLens.set(List(kafkaContainer.bootstrapServers)) andThen clientIdLens.set("my-producer") andThen maxInFlightRequestsPerConnectionLens.set(1)

      val producer: KafkaProducer[String, String] = KafkaProducer[String, String](blah(KafkaProducerConfig.default), io)

      /////////////////////////////////////////////////

      val bootstrapServers2Lens: Lens[KafkaConsumerConfig, List[String]] = GenLens[KafkaConsumerConfig](_.bootstrapServers)
      val groupIdLens: Lens[KafkaConsumerConfig, String] = GenLens[KafkaConsumerConfig](_.groupId)
      val enableAutoCommitLens: Lens[KafkaConsumerConfig, Boolean] = GenLens[KafkaConsumerConfig](_.enableAutoCommit)

      val y = bootstrapServers2Lens.set(List(kafkaContainer.bootstrapServers)) andThen groupIdLens.set("kafka-tests") andThen enableAutoCommitLens.set(true)

      val consumer: Task[KafkaConsumer[String, String]] =
        KafkaConsumerObservable.createConsumer[String, String](y(KafkaConsumerConfig.default), List("my-topic"))
          .map { c =>
            c.poll(0)
            c.seekToBeginning(c.assignment())
            c
          }

      ////////////////////////////////////////////////


      //println(v.runSyncUnsafe())

      val messages = for {
        r <- producer.send("my-topic", "blah blah")
        xxx <- consumer.map(_.poll(3000)).map(_.iterator.asScala.toVector.map(_.value))
      } yield xxx

      println(messages.runSyncUnsafe())
    }
  }
}