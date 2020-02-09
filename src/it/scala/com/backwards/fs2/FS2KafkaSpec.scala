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
import org.apache.kafka.clients.consumer.{ConsumerRecord, ConsumerRecords, KafkaConsumer}
import scala.jdk.CollectionConverters._

class FS2KafkaSpec extends AnyWordSpec with Matchers with DockerComposeFixture {
  val dockerCompose: DockerCompose =
    DockerCompose("kafka", Seq(Paths.get("src", "it", "resources", "docker-compose.yml")))

  implicit val io = Scheduler.io("monix-kafka-tests")

  "" should {
    "" in {
      dockerCompose.services must contain only("zookeeper", "kafka")

      /////////////////////////////////////////////////


      val bootstrapServers2Lens: Lens[KafkaConsumerConfig, List[String]] = GenLens[KafkaConsumerConfig](_.bootstrapServers)
      val groupIdLens: Lens[KafkaConsumerConfig, String] = GenLens[KafkaConsumerConfig](_.groupId)
      val enableAutoCommitLens: Lens[KafkaConsumerConfig, Boolean] = GenLens[KafkaConsumerConfig](_.enableAutoCommit)

      val y = bootstrapServers2Lens.set(List("localhost:9092")) andThen groupIdLens.set("kafka-tests") andThen enableAutoCommitLens.set(true)

      val consumerTask =
        KafkaConsumerObservable.createConsumer[String, String](y(KafkaConsumerConfig.default), List("my-topic")).executeOn(io)

      val consumer = Await.result(consumerTask.runToFuture, 60.seconds)
      consumer.poll(10.seconds.toMillis)

      ////////////////////////////////////////////////

      val bootstrapServersLens: Lens[KafkaProducerConfig, List[String]] = GenLens[KafkaProducerConfig](_.bootstrapServers)
      val clientIdLens: Lens[KafkaProducerConfig, String] = GenLens[KafkaProducerConfig](_.clientId)
      val maxInFlightRequestsPerConnectionLens: Lens[KafkaProducerConfig, Int] = GenLens[KafkaProducerConfig](_.maxInFlightRequestsPerConnection)

      val blah: KafkaProducerConfig => KafkaProducerConfig = bootstrapServersLens.set(List("localhost:9092")) andThen clientIdLens.set("my-producer") andThen maxInFlightRequestsPerConnectionLens.set(1)

      val producer: KafkaProducer[String, String] = KafkaProducer[String, String](blah(KafkaProducerConfig.default), io)

      val Some(recordMetadata) = producer.send("my-topic", "blah blah").runSyncUnsafe()
      println(recordMetadata)


      ////////////////////

      val records = consumer.poll(10.seconds.toMillis).asScala.map(_.value()).toList
      println(records)

    }
  }
}