package com.backwards.fp.kleisli

import scala.util.Random
import cats.data.Kleisli
import cats.effect.IO
import cats.effect.unsafe.implicits.global
import cats.syntax.all._
import org.scalatest.matchers.must.Matchers
import org.scalatest.wordspec.AnyWordSpec

class KleisliSpec extends AnyWordSpec with Matchers {
  val random: Random.type = scala.util.Random

  "Functional composition" should {
    "operate on simple functions" in {
      val generate: Unit => Int =
        _ => random.nextInt(100)

      val process: Int => Int =
        _ * math.Pi toInt

      val save: Int => Boolean =
        _ => true

      val combine: Unit => Boolean =
        generate andThen process andThen save

      combine(()) mustBe true
    }

    "operate on monadic functions" in {
      val generate: Unit => IO[Int] =
        _ => IO.pure(random.nextInt(100))

      val process: Int => IO[Double] =
        num => IO.pure(num * math.Pi)

      val save: Double => IO[Boolean] =
        _ => IO.pure(true)

      val combine1: Kleisli[IO, Unit, Boolean] =
        Kleisli(generate) >>> Kleisli(process) >>> Kleisli(save)

      combine1(()).unsafeRunSync() mustBe true

      val combine2: Kleisli[IO, Unit, Boolean] =
        Kleisli(generate) andThen process andThen save

      combine2(()).unsafeRunSync() mustBe true
    }
  }

  "Kleisli" should {
    type Config = String
    type Result = String

    def getConfig: IO[Config] = IO("config")

    def serviceCall(c: Config): IO[Result] = IO("result")

    "without" in {
      def program: IO[Result] = for {
        config <- getConfig
        result <- serviceCall(config)
      } yield result

      val v: Result = program.unsafeRunSync()

      println(v)
    }

    "with IO" in {
      def program: Kleisli[IO, Config, Result] = for {
        config <- Kleisli.ask[IO, Config]
        result <- Kleisli.liftF(serviceCall(config))
      } yield result

      val v: IO[Result] = getConfig.flatMap(program.run)

      println(v.unsafeRunSync())
    }

    "with Option" in {
      import cats.instances.option._

      def getConfig: Option[Config] = Option("config")

      def serviceCall(c: Config): Option[Result] = Option("result")

      def program: Kleisli[Option, Config, Result] = for {
        config <- Kleisli.ask[Option, Config]
        result <- Kleisli.liftF(serviceCall(config))
      } yield result

      val v: Option[Result] = getConfig.flatMap(program.run)

      println(v)
    }

    "with Option, though a bit more explicitly" in {
      import cats.instances.option._

      def getConfig: Option[Config] = Option("config")

      def serviceCall(c: Config): Option[Result] = Option("result")

      val v: Option[Result] = getConfig.flatMap { config =>
        Kleisli.ask[Option, Config].flatMap { config =>
          Kleisli.liftF(serviceCall(config))
        }.run(config)
      }

      println(v)
    }
  }
}