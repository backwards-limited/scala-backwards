package com.backwards.essentialeffects

case class MyIO[A](unsafeRun: () => A) {
  def map[B](f: A => B): MyIO[B] =
    MyIO(() => f(unsafeRun()))

  def flatMap[B](f: A => MyIO[B]): MyIO[B] =
    MyIO(() => f(unsafeRun()).unsafeRun())
}
object MyIO {
  def putStr(s: => String): MyIO[Unit] =
    MyIO(() => println(s))
}

object Printing extends App {
  import MyIO._

  val hello: MyIO[Unit] = putStr("hello!")
  val world: MyIO[Unit] = putStr("world!")

  val helloWorld: MyIO[Unit] =
    for {
      _ <- hello
      _ <- world
    } yield ()

  helloWorld.unsafeRun()
}