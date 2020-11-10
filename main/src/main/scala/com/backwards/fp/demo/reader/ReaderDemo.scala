package com.backwards.fp.demo.reader

import cats.data.Reader

/**
 * A monad is a mechanism for sequencing computations.
 * Monads provide a pattern for abstracting over the effects of these computations, allowing them to be composed into larger programs.
 *
 * Reader Monad:
 * Provides a functional mechanism for implementing dependency injection.
 */
object ReaderDemo extends App {
  case class Config(name: String, age: Int)

  // Programs that depend on this configuration are effectively a function from Config to some type A

  type ConfigReader[A] = Reader[Config, A]

  def greet(salutation: String): ConfigReader[String] =
    Reader(cfg => s"$salutation ${cfg.name}")

  def validAge: ConfigReader[Int] =
    Reader(cfg => math.abs(cfg.age))

  // Because these programs are both expressed using the Reader monad, we can use them as the building blocks of larger programs (which are themselves Readers).

  import cats.syntax.applicative._ // Allows us to use `pure`

  def greeting: ConfigReader[String] = for {
    g <- greet("Hi,")
    a <- validAge
    p <- (if (a < 18) "a child" else "an adult").pure[ConfigReader]
  } yield s"$g; you are $p."

  // The program can finally be run by supplying a concrete Config instance to the Reader‘s run method.

  val myCfg = Config("Mr Holmes", -37)

  println(greeting.run(myCfg)) // Hi, Mr Holmes; you are an adult.
}