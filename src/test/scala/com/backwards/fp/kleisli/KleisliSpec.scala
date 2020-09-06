package com.backwards.fp.kleisli

import scala.util.Random
import cats.data.Kleisli
import cats.effect.IO
import org.scalatest.matchers.must.Matchers
import org.scalatest.wordspec.AnyWordSpec
import cats.arrow.Arrow.ops.toAllArrowOps

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

      combine() mustBe true
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
}