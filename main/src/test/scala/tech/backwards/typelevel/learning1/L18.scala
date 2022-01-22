package tech.backwards.typelevel.learning1

import shapeless._
import shapeless.ops.hlist._
import shapeless.syntax.singleton._

/**
 * Case class to richer case class migration
 */
object L18 extends App {

  case class Document(name: String, size: Int)

  case class PersistedDocument(name: String, id: Long, size: Int, createdBy: String)

  class Build[Target /*PersistedDocument*/ ] {
    def from[
      Source,                       // Document
      TargetRepr <: HList,          // PersistedDocument HList
      SourceRepr <: HList,          // Document HList
      Fields <: HList,              // fields
      SourceWithFieldsRepr <: HList // field + Document HList
    ](source: Source, fields: Fields)(
      // PersistedDocument representation String :: Int :: String :: HNil
      implicit targetGen: LabelledGeneric.Aux[Target, TargetRepr],

      // Document representation String :: Int :: HNil
      sourceGen: LabelledGeneric.Aux[Source, SourceRepr],

      // Document with added new field String :: String :: Int :: HNil
      // Take into account that we can add fields only as a head of HList
      sourceWithFieldsRepr: Prepend.Aux[Fields, SourceRepr, SourceWithFieldsRepr],

      // Reordered representation of Document with add field String :: Int :: String:: HNil
      // by PersistedDocument representation pattern
      align: Align[SourceWithFieldsRepr, TargetRepr]
     ): Target = {

      targetGen.from(          // <- HList into PersistedDocument
        sourceWithFieldsRepr(  // <- Add new Fields to the Document representation
          fields,
          sourceGen.to(source) // <- Document into HList
        ).align[TargetRepr]    // <- Reorder result as in PersistedDocument
      )
    }
  }

  object Build {
    def apply[A] = new Build[A]
  }

  val result =
    Build[PersistedDocument].from(Document("hello", 42), Symbol("id") ->> 17L :: Symbol("createdBy") ->> "user" :: HNil)

  println(result)
}