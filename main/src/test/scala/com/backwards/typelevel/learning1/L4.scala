package com.backwards.typelevel.learning1

import shapeless._

/**
 * HList to case class and tuple conversion and vise versa via Generic
 */
object L4 extends App {
  case class Document(path: String, size: Long, readOnly: Boolean)

  val documentGen = Generic[Document]
  val document = Document("xyz", 42L, readOnly = true)

  val documentRepr = documentGen.to(document)
  val doc1 = documentGen.from(documentGen.to(document))
  val doc2 = documentGen.from("abc" :: 42L :: true :: HNil)

  case class PersistedDocument(createdBy: String, path: String, size: Long, readOnly: Boolean)

  val persistedDocumentGen = Generic[PersistedDocument]

  val persistedDocument = persistedDocumentGen.from("user@company.com" :: documentRepr)
  println(persistedDocument)

  val tupleGen = Generic[(String, Int, Boolean)]
  println(tupleGen.to(("Hello", 42, true)))
  println(tupleGen.from("Hello" :: 42 :: true :: HNil))
}