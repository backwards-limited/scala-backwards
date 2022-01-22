package tech.backwards.macros

import scala.meta._

object LetsGo extends App {
  println("..... made it")

  val program = """object Main extends App { print("Hello!") }"""
  val tree: Source = program.parse[Source].get

  println(tree.syntax)
}