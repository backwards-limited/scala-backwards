package com.backwards.fp.free

import scala.concurrent.ExecutionContext.Implicits.global
import scala.io.StdIn
import cats.data.EitherK
import cats.free.Free
import cats.implicits.catsSyntaxApply
import cats.{Id, InjectK, ~>}

/**
 * [[https://www.slideshare.net/hermannhueck/composing-an-app-with-free-monads-using-cats]]
 */
object FreeMonadComposedApp extends App {
  type AppDSL[A] = EitherK[Interact, KVStore, A]

  def program(implicit I: InteractOps[AppDSL], KVS: KVStoreOps[AppDSL]): Free[AppDSL, (String, Option[Int])] =
    for {
      name <- I.ask("\nWhat's your name?")
      age <- I.ask("What's your age?")
      _ <- KVS.put(name, age.toInt)
      _ <- I.printline(s"Hello $name! Your age is $age.")
      optAge <- KVS.get(name)
    } yield (name, optAge)

  val interpreter: AppDSL ~> Id = ConsoleInterpreter or KVStoreInterpreter

  // Use Free.foldMap to execute the program with a specific interpreter - foldMap internally uses trampolining which makes Free monads stack safe.
  val (name, age) = program.foldMap(interpreter)
  println(s"$name -> $age")

  /**
   * Interact and DSL
   */
  sealed trait Interact[A]

  final case class Printline(out: String) extends Interact[Unit]

  case object Getline extends Interact[String]

  class InteractOps[F[_]](implicit I: InjectK[Interact, F]) {
    def printline(out: String): Free[F, Unit] = Free.liftInject(Printline(out))

    def getline: Free[F, String] = Free.liftInject(Getline)

    def ask(prompt: String): Free[F, String] = printline(prompt) *> getline
  }

  object InteractOps {
    implicit def interactOps[F[_]](implicit I: InjectK[Interact, F]): InteractOps[F] = new InteractOps[F]
  }

  /**
   * KVStore and DSL
   */
  sealed trait KVStore[A]

  final case class Put(key: String, value: Int) extends KVStore[Unit]

  final case class Get(key: String) extends KVStore[Option[Int]]

  final case class Delete(key: String) extends KVStore[Option[Int]]

  class KVStoreOps[F[_]](implicit KV: InjectK[KVStore, F]) {
    def put(key: String, value: Int): Free[F, Unit] = Free.liftInject(Put(key, value))

    def get(key: String): Free[F, Option[Int]] = Free.liftInject(Get(key))

    def delete(key: String): Free[F, Option[Int]] = Free.liftInject(Delete(key))
  }

  object KVStoreOps {
    implicit def kvsOps[F[_]](implicit I: InjectK[KVStore, F]): KVStoreOps[F] = new KVStoreOps[F]
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

  object KVStoreInterpreter extends (KVStore ~> Id) {
    var kvs: Map[String, Int] = Map.empty

    def apply[A](fa: KVStore[A]): Id[A] = fa match {
      case Put(key, value) =>
        kvs = kvs.updated(key, value)
        ()

      case Get(key) =>
        kvs.get(key)

      case Delete(key) =>
        val value = kvs.get(key)
        kvs = kvs - key
        value
    }
  }

  /**
   * EitherK (= Coproduct)
   *
   * Either is parameterised with 2 types A and B.
   * sealed abstract class Either[+A, +B]
   *
   * EitherK is parameterised with 2 type constructors F[_] and G[_] and a regular type A.
   * It's a case class wrapping a value called run of type Either[F[A], G[A]]
   * final case class EitherK[F[_], G[_], A](run: Either[F[A], G[A]])
   *
   * EitherK is used to define a composed DSL
   */
  object WhatIsEitherK
}