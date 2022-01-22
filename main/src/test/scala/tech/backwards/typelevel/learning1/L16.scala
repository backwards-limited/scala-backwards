package tech.backwards.typelevel.learning1

import shapeless._
import shapeless.syntax.singleton._
import tech.backwards.typelevel._

/**
 * LabelledGeneric hello world
 */
object L16 extends App {

  case class Document(path: String, size: Int, readOnly: Boolean)

  val document = Document("/", 42, readOnly = true)

  val docGen = LabelledGeneric[Document]

  println(document.toRepr)
  println(docGen.from(Symbol("path") ->> "/" :: Symbol("size") ->> 42 :: Symbol("readOnly") ->> true :: HNil))
}