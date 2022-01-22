package tech.backwards.chimney

import org.specs2.mutable.Specification
import cats.implicits._
import io.scalaland.chimney.dsl._
import tech.backwards.catz.validated.{AirtableDQView, AirtableDQViewValidated, Element, Error, FunctionParameterMetaType, Severity, Value, Warning}

object ChimneySpec extends Specification {
  val correctView: AirtableDQView =
    AirtableDQView(
      `Asset Unique ID` = Some("PDC-DQIC-1104"),
      `Severity` = Some("Warning"),
      `Asset Name` = Some("Name"),
      `Values` = None,
      `Record Identifier Ref` = Some(List("Data Universe.Raw.Json files.Eclipse.PolicyLine.PolicyLineRef")),
      `Function Ref` = Some(List("E1 not before E2")),
      `Arity Ref` = Some(List(2)),
      `Data Element Ref` = Some(List(
        "Data Universe.Raw.Json files.Eclipse.PolicyLine.SignedOrder",
        "Data Universe.Raw.Json files.Eclipse.PolicyLine.LineStatus"
      )),
      `Reason Type` = Some("Bob"),
      `Raw Source` = Some(List("Eclipse"))
    )

  "Blah" should {
    "blah" in {
      val severity: PartialFunction[String, Severity] = {
        case "Warning" => Warning
        case "Error" => Error
      }

      val metaType: String => FunctionParameterMetaType =
        _.toUpperCase match {
          case "E" => Element
          case "V" => Value
          case x => throw new Exception
        }

      val metaTypePattern = """([a-zA-Z])\d+""".r

      val v =
        correctView
          .into[AirtableDQViewValidated]
          .withFieldComputed(_.assetUniqueID, _.`Asset Unique ID`.getOrElse(throw new Exception))
          .withFieldComputed(_.severity, _.`Severity`.flatMap(severity.lift).getOrElse(throw new Exception))
          .withFieldComputed(_.assetName, _.`Asset Name`.getOrElse(throw new Exception))
          .withFieldComputed(_.recordIdentifier, _.`Record Identifier Ref`.flatMap(_.headOption).getOrElse(throw new Exception))
          .withFieldComputed(_.elements, _.`Data Element Ref`.flatTraverse(_.map(_.split("\\.").lastOption)).flattenOption)
          .withFieldComputed(_.tableIdentifier, _.`Record Identifier Ref`.flatMap(_.headOption).map(_.split("\\.").dropRight(1).mkString(".")).getOrElse(throw new Exception))
          .withFieldComputed(_.tableName, _.`Record Identifier Ref`.flatMap(_.headOption).flatMap(_.split("\\.").dropRight(1).lastOption).getOrElse(throw new Exception))
          .withFieldComputed(_.values, _.Values.map(_.split(",").toList).getOrElse(Nil))
          .withFieldComputed(_.function, _.`Function Ref`.flatMap(_.headOption).getOrElse(throw new Exception))
          .withFieldComputed(_.functionParameterMetaTypes, _.`Function Ref`.flatMap(_.headOption.map(metaTypePattern.findAllMatchIn).map(_.toList.map(_.group(1)).map(metaType))).getOrElse(Nil))
          .withFieldComputed(_.arity, _.`Arity Ref`.flatMap(_.headOption).getOrElse(throw new Exception))
          .withFieldComputed(_.reasonType, _.`Reason Type`.getOrElse(throw new Exception))
          .withFieldComputed(_.rawSource, _.`Raw Source`.map(_.distinct).collect { case List(r) => r }.getOrElse(throw new Exception))
          .transform

      println(v)

      ok
    }
  }
}