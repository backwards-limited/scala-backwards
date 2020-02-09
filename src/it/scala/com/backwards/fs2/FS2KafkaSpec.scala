package com.backwards.fs2

import java.nio.file.Paths
import java.util.concurrent.TimeUnit
import scala.concurrent.Await
import scala.concurrent.duration.FiniteDuration
import cats.implicits._
import monix.execution.{CancelableFuture, Scheduler}
import monix.kafka
import monix.kafka.{KafkaConsumerConfig, KafkaConsumerObservable, KafkaProducer, KafkaProducerConfig}
import monix.reactive.Observable
import monocle.Lens
import monocle.macros.GenLens
import org.scalatest.matchers.must.Matchers
import org.scalatest.wordspec.AnyWordSpec
import com.backwards.docker.{DockerCompose, DockerComposeFixture}
import monocle.macros.syntax.lens._
import scala.concurrent.duration._
import monix.eval.Task
import org.apache.kafka.clients.consumer.{ConsumerConfig, ConsumerRecord, ConsumerRecords, KafkaConsumer}
import scala.jdk.CollectionConverters._

/**
 * kafkacat -P -b localhost:9092 -t my-topic
 *
 * kafkacat -C -b localhost:9092 -t my-topic -o beginning
 */
class FS2KafkaSpec extends AnyWordSpec with Matchers with DockerComposeFixture {
  val dockerCompose: DockerCompose =
    DockerCompose("kafka", Seq(Paths.get("src", "it", "resources", "docker-compose.yml")))

  implicit val io = Scheduler.io("monix-kafka-tests")

  "" should {
    "" in {
      dockerCompose.services must contain only("zookeeper", "kafka")

      /////////////////////////////////////////////////

      val bootstrapServersLens: Lens[KafkaProducerConfig, List[String]] = GenLens[KafkaProducerConfig](_.bootstrapServers)
      val clientIdLens: Lens[KafkaProducerConfig, String] = GenLens[KafkaProducerConfig](_.clientId)
      val maxInFlightRequestsPerConnectionLens: Lens[KafkaProducerConfig, Int] = GenLens[KafkaProducerConfig](_.maxInFlightRequestsPerConnection)

      val blah: KafkaProducerConfig => KafkaProducerConfig = bootstrapServersLens.set(List("localhost:9092")) andThen clientIdLens.set("my-producer") andThen maxInFlightRequestsPerConnectionLens.set(1)

      val producer: KafkaProducer[String, String] = KafkaProducer[String, String](blah(KafkaProducerConfig.default), io)

      val Some(recordMetadata) = producer.send("my-topic", "blah blah").runSyncUnsafe()
      println(recordMetadata)

      /////////////////////////////////////////////////


      val bootstrapServers2Lens: Lens[KafkaConsumerConfig, List[String]] = GenLens[KafkaConsumerConfig](_.bootstrapServers)
      val groupIdLens: Lens[KafkaConsumerConfig, String] = GenLens[KafkaConsumerConfig](_.groupId)
      val enableAutoCommitLens: Lens[KafkaConsumerConfig, Boolean] = GenLens[KafkaConsumerConfig](_.enableAutoCommit)
      val propertiesLens: Lens[KafkaConsumerConfig, Map[String, String]] = GenLens[KafkaConsumerConfig](_.properties)

      val y = bootstrapServers2Lens.set(List("localhost:9092")) andThen groupIdLens.set("kafka-tests") andThen enableAutoCommitLens.set(true) andThen propertiesLens.modify(_ + (ConsumerConfig.AUTO_OFFSET_RESET_CONFIG -> "earliest"))

      val v: Task[Vector[String]] =
        KafkaConsumerObservable.createConsumer[String, String](y(KafkaConsumerConfig.default), List("my-topic"))
          .map { c =>
            c.poll(0)
            c.seekToBeginning(c.assignment())
            c
          }
          .map(_.poll(3000)).map(_.iterator.asScala.toVector.map(_.value))


      ////////////////////////////////////////////////


      println(v.runSyncUnsafe())

    }
  }
}