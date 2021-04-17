package com.backwards.catz.validated

import scala.util.matching.Regex
import cats.data.{Chain, NonEmptyChain, ValidatedNec}
import org.specs2.mutable.Specification
import cats.data.Validated._
import cats.implicits._
import monocle.macros.GenLens
import monocle.{Lens, Optional}

object ValidatedSpec extends Specification {
  import AirtableDQViewValidated._

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

  "AirtableDQViewValidated" should {
    "accept a valid AirtableDQView" in {
      AirtableDQViewValidated.applyOrThrow(correctView) must_=== AirtableDQViewValidated(
        assetUniqueID = "PDC-DQIC-1104",
        severity = Warning,
        assetName = "Name",
        recordIdentifier = "Data Universe.Raw.Json files.Eclipse.PolicyLine.PolicyLineRef",
        elements = List("SignedOrder", "LineStatus"),
        tableIdentifier = "Data Universe.Raw.Json files.Eclipse.PolicyLine",
        tableName = "PolicyLine",
        values = Nil,
        function = "E1 not before E2",
        functionParameterMetaTypes = List(Element, Element),
        arity = 2,
        reasonType = "Bob",
        rawSource = "Eclipse"
      )
    }

    "reject an invalid AirtableDQView" in {
      import AirtableDQView._

      val Invalid(Chain(error1: String, error2: String)) =
        AirtableDQViewValidated((assetUniqueIdL.set(None) andThen severityL.set(None))(correctView))

      error1 must contain("Asset Unique ID")
      error2 must contain("Severity")
    }

    "produce valid function parameter meta type" in {
      metaType("E") must_=== Element

      metaType.lift("X") must beNone
    }
  }
}

sealed trait Severity

sealed trait RegularSeverity extends Severity

case object Warning extends RegularSeverity

case object Error extends RegularSeverity

case object ItemError extends Severity

object Severity {
  val apply: PartialFunction[String, Severity] = {
    case "Warning" => Warning
    case "Error" => Error
    case "ItemError" => ItemError
  }
}

sealed trait FunctionParameterMetaType

case object Element extends FunctionParameterMetaType

case object Value extends FunctionParameterMetaType

final case class AirtableDQView(
  `Asset Unique ID`: Option[String],
  `Severity`: Option[String],
  `Asset Name`: Option[String],
  Values: Option[String],
  `Record Identifier Ref`: Option[List[String]],
  `Function Ref`: Option[List[String]],
  `Arity Ref`: Option[List[Int]],
  `Data Element Ref`: Option[List[String]],
  `Reason Type`: Option[String],
  `Raw Source`: Option[List[String]]
)

object AirtableDQView {
  val assetUniqueIdL: Lens[AirtableDQView, Option[String]] =
    GenLens[AirtableDQView](_.`Asset Unique ID`)

  val assetUniqueIdO: Optional[AirtableDQView, String] =
    Optional[AirtableDQView, String](_.`Asset Unique ID`)(a => _.copy(`Asset Unique ID` = Some(a)))

  val severityL: Lens[AirtableDQView, Option[String]] =
    GenLens[AirtableDQView](_.`Severity`)

  val severityO: Optional[AirtableDQView, String] =
    Optional[AirtableDQView, String](_.`Severity`)(s => _.copy(`Severity` = Some(s)))
}

final case class AirtableDQViewValidated(
  assetUniqueID: String,
  severity: Severity,
  assetName: String,
  recordIdentifier: String,
  elements: List[String],
  tableIdentifier: String,
  tableName: String,
  values: List[String],
  function: String,
  functionParameterMetaTypes: List[FunctionParameterMetaType],
  arity: Int,
  reasonType: String,
  rawSource: String
)

object AirtableDQViewValidated {
  type ValidationResult[A] = ValidatedNec[String, A]

  val severity: PartialFunction[String, Severity] = {
    case "Warning" => Warning
    case "Error" => Error
  }

  val metaType: PartialFunction[String, FunctionParameterMetaType] = {
    val upper: PartialFunction[String, String] =
      _.toUpperCase

    upper andThen {
      case "E" => Element
      case "V" => Value
    }
  }

  val metaTypePattern: Regex =
    """([a-zA-Z])\d+""".r

  val failure: NonEmptyChain[String] => Exception =
    e => new Exception(e.mkString_(", "))

  def applyOrThrow(dqView: AirtableDQView): AirtableDQViewValidated =
    apply(dqView).valueOr(failure.andThen(throw _))

  def apply(dqView: AirtableDQView): ValidationResult[AirtableDQViewValidated] = {
    val noValues: String => String =
      "No value(s) returned from Airtable API for " + _

    val assetUniqueID: AirtableDQView => ValidationResult[String] =
      _.`Asset Unique ID` map(_.validNec) getOrElse noValues("Asset Unique ID").invalidNec

    val severity: AirtableDQView => ValidationResult[Severity] =
      _.`Severity`.collect {
        case "Warning" => Warning
        case "Error" => Error
      } map(_.validNec) getOrElse "Severity not given as either 'Warning' or 'Error'".invalidNec

    val assetName: AirtableDQView => ValidationResult[String] =
      _.`Asset Name` map(_.validNec) getOrElse noValues("Asset Name").invalidNec


    val usingRecordIdentifier: (String => ValidationResult[String]) => AirtableDQView => ValidationResult[String] =
      f => _.`Record Identifier Ref`.collectFold {
        case h :: Nil => f(h)
        case _ => "TODO".invalidNec
      }

    val recordIdentifier: AirtableDQView => ValidationResult[String] =
      usingRecordIdentifier(_.validNec)

    val elements: AirtableDQView => ValidationResult[List[String]] =
      _.`Data Element Ref`.flatTraverse(_.map(_.split("\\.").lastOption)).flattenOption.validNec

    val tableIdentifier: AirtableDQView => ValidationResult[String] =
      usingRecordIdentifier(_.split("\\.").dropRight(1).mkString(".").validNec)

    val tableName: AirtableDQView => ValidationResult[String] =
      usingRecordIdentifier(_.split("\\.").dropRight(1).lastOption.map(_.validNec).getOrElse("Missing table name".invalidNec))

    val values: AirtableDQView => ValidationResult[List[String]] =
      _.Values.map(_.split(",").toList).getOrElse(Nil).validNec

    val function: AirtableDQView => ValidationResult[String] =
      _.`Function Ref` flatMap(_.headOption) map(_.validNec) getOrElse noValues("Function").invalidNec

    val functionParameterMetaTypes: AirtableDQView => ValidationResult[List[FunctionParameterMetaType]] =
      _.`Function Ref`.flatMap(_.headOption).map(metaTypePattern.findAllMatchIn).map(_.toList.map(_.group(1))).collectFold {
        case Nil =>
          List.empty[FunctionParameterMetaType].validNec[String]
        case ms =>
          val validMs: List[ValidationResult[FunctionParameterMetaType]] =
            ms.map(metaType.lift).map {
              case Some(m) => m.validNec
              case _ => "TODO".invalidNec
            }

          validMs.sequence
      }

    val arity: AirtableDQView => ValidationResult[Int] =
      _.`Arity Ref`.collectFold {
        case List(a) => a.validNec
        case _ => "TODO".invalidNec
      }

    val reasonType: AirtableDQView => ValidationResult[String] =
      _.`Reason Type`.map(_.validNec).getOrElse(noValues("Reason Type").invalidNec)

    val rawSource: AirtableDQView => ValidationResult[String] =
      _.`Raw Source`.map(_.distinct).collectFold {
        case List(r) => r.validNec
        case _ => "TODO".invalidNec
      }

    (
      assetUniqueID(dqView),
      severity(dqView),
      assetName(dqView),
      recordIdentifier(dqView),
      elements(dqView),
      tableIdentifier(dqView),
      tableName(dqView),
      values(dqView),
      function(dqView),
      functionParameterMetaTypes(dqView),
      arity(dqView),
      reasonType(dqView),
      rawSource(dqView)
      ).mapN(AirtableDQViewValidated.apply)
  }
}