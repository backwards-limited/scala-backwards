package tech.backwards.fp.referentialtransparency

import scala.concurrent.Future
import cats.effect.unsafe.implicits.global
import cats.effect.{IO, Ref}
import cats.implicits._
import org.scalatest.matchers.must.Matchers
import org.scalatest.wordspec.AnyWordSpec

/**
  * https://vimeo.com/294736344
  */
class ReferentialTransparencySpec extends AnyWordSpec with Matchers {
  "Referential transparency (simple)" should {
    "allow to substitute and yet nothing will change" in {
      val expr = 123
      (expr, expr)

      // Substitute
      (123, 123)
    }

    "not be valid when substitution will change" in {
      val expr: Unit = println("Hi")
      (expr, expr) // <-- print has already happened, and only once

      // Substitute
      (println("Hi"), println("Hi")) // print happens twice
    }

    "not be valid again when substitution will change" in {
      import scala.concurrent.ExecutionContext.Implicits.global

      val expr: Unit = Future(println("Hi"))
      (expr, expr) // <-- print will happen only once

      // Substitute
      (Future(println("Hi")), Future(println("Hi"))) // print will happen twice
    }
  }

  "Referential transparency with IO, which represents the intention to perform a side effect" should {
    "be simple" in {
      val expr = IO(println("Hi"))
      (expr, expr)

      // Substitute
      (IO(println("Hi")), IO(println("Hi")))
    }

    "be more interesting" in {
      import cats.effect.unsafe.implicits.global

      val counter: IO[Ref[IO, Int]] = Ref.of[IO, Int](0)

      val program: IO[Int] = for {
        _ <- counter.flatMap(_.update(_ + 1))
        v <- counter.flatMap(_.get)
      } yield v

      // Intuitively one might believe the result will be 1.
      // Not so, and this is because of referential transparency
      program.unsafeRunSync() mustBe 0

      val programProof: IO[Int] = for {
        _ <- Ref.of[IO, Int](0).flatMap(_.update(_ + 1))
        v <- Ref.of[IO, Int](0).flatMap(_.get)
      } yield v

      programProof.unsafeRunSync() mustBe 0

      // But how can this be useful? How to have the program act upon the same Ref?
      // ... Pass one in.
      val programFinal: Ref[IO, Int] => IO[Int] =
        ref => for {
          _ <- ref.update(_ + 1)
          v <- ref.get
        } yield v

      counter.flatMap(programFinal).unsafeRunSync() mustBe 1
    }
  }
}