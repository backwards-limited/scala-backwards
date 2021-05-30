package com.backwards.fp.free

import scala.collection.mutable.ListBuffer
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.DurationInt
import scala.concurrent.{Await, Future}
import scala.io.StdIn
import cats.free.Free
import cats.implicits.catsSyntaxApply
import cats.{Id, InjectK, ~>}

/**
 * [[https://www.slideshare.net/hermannhueck/composing-an-app-with-free-monads-using-cats]]
 */
object FreeMonadApp extends App {
  import Algebra._
  import Dsl._

  def program1: Free[Interact, (String, Int)] =
    for {
      _ <- printline("\nWhat's your name?")
      name <- getline
      _ <- printline("What's your age?")
      age <- getline
      _ <- printline(s"Hello $name! Your age is $age.")
    } yield (name, age.toInt)

  // Use Free.foldMap to execute the program with a specific interpreter - foldMap internally uses trampolining which makes Free monads stack safe.
  val (name1, age1) = program1.foldMap(ConsoleInterpreter)
  println(s"Program1 ConsoleInterpreter: $name1 -> $age1")

  def program2: Free[Interact, (String, Int)] =
    for {
      name <- ask("\nWhat's your name?")
      age <- ask("What's your age?")
      _ <- printline(s"Hello $name! Your age is $age.")
    } yield (name, age.toInt)

  val (name2, age2) = program2.foldMap(ConsoleInterpreter)
  println(s"Program2 ConsoleInterpreter: $name2 -> $age2")

  val result: Future[(String, Int)] = program2.foldMap(AsyncInterpreter)
  println(s"Program2 AsyncInterpreter: ${Await.result(result, 10 seconds)}")

  val (name3, age3) = program2.foldMap(new TestInterpreter(ListBuffer.empty[String], ListBuffer.empty[String]))
  println(s"Program2 TestInterpreter: $name3 -> $age3")

  def program4(implicit I: InteractOps[Interact]): Free[Interact, (String, Int)] =
    for {
      name <- I.ask("\nWhat's your name?")
      age <- I.ask("What's your age?")
      _ <- I.printline(s"Hello $name! Your age is $age.")
    } yield (name, age.toInt)

  val (name4, age4) = program4.foldMap(ConsoleInterpreter)
  println(s"Program4 ConsoleInterpreter: $name4 -> $age4")

  /**
   * Algebra as an ADT - 1 type paramater for the return type
   */
  object Algebra {
    sealed trait Interact[A]

    final case class Printline(out: String) extends Interact[Unit]

    case object Getline extends Interact[String]
  }

  /**
   * DSL - Lift your ADT into the Free Monad
   */
  object Dsl {
    def printline(out: String): Free[Interact, Unit] = Free.liftF(Printline(out))

    def getline: Free[Interact, String] = Free.liftF(Getline)

    def ask(prompt: String): Free[Interact, String] = printline(prompt) *> getline
  }

  /**
   * DSL - Using Free.inject (instead of Free.liftF)
   */
  class InteractOps[F[_]](implicit I: InjectK[Interact, F]) {
    def printline(out: String): Free[F, Unit] = Free.liftInject(Printline(out))

    def getline: Free[F, String] = Free.liftInject(Getline)

    def ask(prompt: String): Free[F, String] = printline(prompt) *> getline
  }

  object InteractOps {
    implicit def interactOps[F[_]](implicit I: InjectK[Interact, F]): InteractOps[F] = new InteractOps[F]
  }

  /**
   * Every interpreter is/extends a natural transformation from your DSL monad (Interact) to a target monad (Id).
   * ~> is a shortcut for the natural transformation FunctionK
   */
  object ConsoleInterpreter extends (Interact ~> Id) {
    def apply[A](fa: Interact[A]): Id[A] = fa match {
      case Printline(out) =>
        println(out)
        ()

      case Getline =>
        val in = StdIn.readLine()
        in
    }
  }

  object AsyncInterpreter extends (Interact ~> Future) {
    def apply[A](fa: Interact[A]): Future[A] = fa match {
      case Printline(out) =>
        Future successful println(out)

      case Getline =>
        Future successful StdIn.readLine()
    }
  }

  class TestInterpreter(inputs: ListBuffer[String], outputs: ListBuffer[String]) extends (Interact ~> Id) {
    def apply[A](fa: Interact[A]): Id[A] = fa match {
      case Printline(out) =>
        println(out)
        ()

      case Getline =>
        val in = StdIn.readLine()
        in
    }
  }

  /**
   * Function1[-A, +B] takes an A and returns a B.
   * Shortcut: A => B
   * {{{
   *    trait Function1[-A, +B] {
   *      def apply(a: A): B
   *    }
   * }}}
   *
   * FunctionK[F[_], G[_]] takes a F[A] and returns a G[A].
   * Shortcut: F ~> G
   * {{{
   *    trait FunctionK[F[_], G[_]] {
   *      def apply[A](fa: F[A]): G[A]
   *    }
   * }}}
   *
   */
  object WhatIsFunctionK
}