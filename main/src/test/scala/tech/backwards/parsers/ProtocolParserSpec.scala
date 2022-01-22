package tech.backwards.parsers

import scala.util.parsing.combinator._
import cats.implicits._
import org.specs2.matcher.ParserMatchers
import org.specs2.mutable.Specification
import org.specs2.specification.core.Fragment

/**
 * [[https://objectcomputing.com/resources/publications/sett/april-2016-protocol-parsing-in-scala-part-ii Protocol Parsing]]
 *
 * EBNF:
 *  - name	:=	%NAME nl qstring nl
 *  - address	:=	%ADDRESS nl int . int . int nl
 *  - hardwareID	:=	%HWID nl hex{8} nl
 *  - devInfo	:=	%DEVINFO < name address hardwareID >
 *  - daqRate	:=	%DR nl (E | P , int) nl
 *  - range	:=	%RANGE nl double , double nl
 *  - type	:=	%TYPE nl int nl
 *  - enabled	:=	%EN nl bool nl
 *  - diagnostic	:=	%DIAGSTART nl <any characters> %DIAGEND nl
 *  - sensor	:=	%SENSOR int < name [daqRate] [range] [type] [enabled] [diagnostic] >
 *  - actuator	:=	%ACTUATOR int < name [range] [type] [enabled] [diagnostic] >
 *  - device	:=	[ devInfo sensor{n} actuator{m} ]
 */
class ProtocolParserSpec extends Specification with ParserMatchers {
  val parsers: ConfigParser.type = ConfigParser

  import ConfigParser._

  "qstring" should {
    "recognize \"fred\"" in {
      qstring must succeedOn("\"fred\"").withResult("fred")
    }

    "not recognize fred\"" in {
      qstring must not succeedOn "fred\""
    }

    "not recognize \"fred" in {
      qstring must failOn("\"fred")
    }
  }

  "name" should {
    "recognize '%NAME\n\"fred\"\n'" in {
      name must succeedOn("%NAME\n\"fred\"\n").withResult(Name("fred"))
    }

    "encode Name(\"fred\") correctly" in {
      Name("fred").toString must_=== "%NAME\n\"fred\"\n"
    }
  }

  "name - using helper function" should {
    def roundtrip[A](e: A, parser: Parser[A]): Fragment =
      "roundtrip " + e.toString.replace("\n", "\\n") in {
        parser must succeedOn(e.toString).withResult(e)
      }

    "roundtrip name" in {
      roundtrip(Name("fred"), name)
    }

    "roundtrip device" in {
      roundtrip(
        Device(
          DevInfo(Name("Weather Station"), Address(1,2,3), HardwareID("0123ABCD")),
          List(
            Sensor(
              1, Name("Temperature"), Some(DaqRate(DaqRatePeriodic(60))),
              Some(Range(-20, 120)), Some(Type(1)), Some(Enabled(true)),
              Some(Diagnostic("I'm broken"))
            ),
            Sensor(
              2, Name("Humidity"), Some(DaqRate(DaqRatePeriodic(300))),
              Some(Range(0, 100)), Some(Type(2)), Some(Enabled(true)),
              None
            ),
            Sensor(
              3, Name("IsRaining"), Some(DaqRate(DaqRateEvent)),
              Some(Range(0, 1)), Some(Type(3)), None, None
            )
          ),
          List(
            Actuator(1, Name("Alarm"), Some(Range(0, 1)), Some(Type(1)), Some(Enabled(true)), None)
          )
        ),
        device
      )
    }
  }
}

object ConfigParser extends RegexParsers {
  override val skipWhitespace = false

  lazy val newLine: Parser[String] = "\n"

  lazy val int: Parser[Int] = "[-+]?[0-9]+".r ^^ (_.toInt)

  lazy val double: Parser[Double] = """[-+]?[0-9]*\.?[0-9]+([eE][-+]?[0-9]+)?""".r ^^ (_.toDouble)

  lazy val qstring: Parser[String] = "\"" ~> ("\"\"" | "[^\"]".r).* <~ "\"" ^^ (_.mkString)

  lazy val bool: Parser[Boolean] = ("0" | "1") ^^ (_.toInt != 0)

  lazy val name: Parser[Name] = "%NAME" ~> newLine ~> qstring <~ newLine ^^ Name

  lazy val address: Parser[Address] = "%ADDRESS" ~> newLine ~> int ~ "." ~ int ~ "." ~ int <~ newLine ^^ {
    case domain ~ _ ~ subnet ~ _ ~ node => Address(domain, subnet, node)
  }

  lazy val hardwareID: Parser[HardwareID] = "%HWID" ~> newLine ~> "[0-9A-F]{8}".r <~ newLine ^^ HardwareID

  lazy val devInfo: Parser[DevInfo] = "%DEVINFO" ~> "<" ~> name ~ address ~ hardwareID <~ ">" ^^ {
    case name ~ address ~ hardwareId => DevInfo(name, address, hardwareId)
  }

  /**
   * The data acquisition rate is of two forms:
   *  - An event that may fire at any time
   *  - A periodic sequence with a fixed interval
   * This may be expressed by first defining the alternatives:
   */
  lazy val daqRateEvent: Parser[DaqRateEvent.type] = "E" ^^ (_ => DaqRateEvent)

  lazy val daqRatePeriodic: Parser[DaqRatePeriodic] = "P," ~> int ^^ DaqRatePeriodic

  lazy val daqRate: Parser[DaqRate] = "%DR" ~> newLine ~> (daqRateEvent | daqRatePeriodic) <~ newLine ^^ DaqRate

  lazy val range: Parser[Range] = "%RANGE" ~> newLine ~> double ~ "," ~ double <~ newLine ^^ {
    case min ~ _ ~ max => Range(min, max)
  }

  lazy val `type`: Parser[Type] = "%TYPE" ~> newLine ~> int <~ newLine ^^ Type

  lazy val enabled: Parser[Enabled] = "%EN" ~> newLine ~> bool <~ newLine ^^ Enabled

  lazy val diagnostic: Parser[Diagnostic] =
    (in: Input) => {
      val source: CharSequence = in.source
      val offset: Int = in.offset
      val start: Int = handleWhiteSpace(source, offset)
      val secStart: String = "%DIAGSTART\n"
      val secEnd: String = "%DIAGEND\n"
      val text: String = source.subSequence(start, source.length).toString

      val iStart: Int = text.indexOf(secStart) // -1 if no match

      if (iStart >= 0) {
        val contents = text.drop(iStart + secStart.length)
        val iEnd = contents.indexOf(secEnd)

        if (iEnd == -1)
          Success(Diagnostic(contents), in.drop(start + secStart.length + contents.length - offset))
        else {
          val strippedContents = contents.take(iEnd)

          Success(Diagnostic(strippedContents), in.drop(start + secStart.length + strippedContents.length + secEnd.length - offset))
        }
      }
      else {
        Failure(s"$secStart not found", in.drop(start - offset))
      }
    }

  lazy val sensor: ConfigParser.Parser[Sensor] =
    "%SENSOR" ~> int ~ "<" ~ name ~ daqRate.? ~ range.? ~ `type`.? ~ enabled.? ~ diagnostic.? <~ ">" ^^ {
      case index ~ _ ~ name ~ daqRate ~ range ~ typ ~ enabled ~ diagnostic =>
        Sensor(index, name, daqRate, range, typ, enabled, diagnostic)
    }

  lazy val actuator: Parser[Actuator] =
    "%ACTUATOR" ~> int ~ "<" ~ name ~ range.? ~ `type`.? ~ enabled.? ~ diagnostic.? <~ ">" ^^ {
      case index ~ _ ~ name ~ range ~ typ ~ enabled ~ diagnostic =>
        Actuator(index, name, range, typ, enabled, diagnostic)
    }

  lazy val device: Parser[Device] = "[" ~> devInfo ~ sensor.* ~ actuator.* <~ "]" ^^ {
    case devInfo ~ sensors ~ actuators => Device(devInfo, sensors, actuators)
  }

  def apply(s: String): Option[Device] =
    parse(device, s) match {
      case Success(matched, _) =>
        Some(matched)
      case Failure(msg, _) =>
        println(s"Parse failure of $s due to: $msg")
        None
      case Error(msg, _) =>
        println(s"Parse error of $s due to: $msg")
        None
    }
}

final case class Name(name: String) {
  override def toString = s"""%NAME\n"$name"\n"""
}

final case class Address(domain: Int, subnet: Int, node: Int) {
  override def toString = s"%ADDRESS\n$domain.$subnet.$node\n"
}

final case class HardwareID(id: String) {
  override def toString = s"%HWID\n$id\n"
}

final case class DevInfo(name: Name, address: Address, hardwareID: HardwareID) {
  override def toString = s"%DEVINFO<$name$address$hardwareID>"
}

trait DaqRateType

case object DaqRateEvent extends DaqRateType {
  override def toString = "E"
}

final case class DaqRatePeriodic(interval: Int) extends DaqRateType {
  override def toString = s"P,$interval"
}

final case class DaqRate(daqRate: DaqRateType) {
  override def toString = s"%DR\n$daqRate\n"
}

final case class Range(min: Double, max: Double) {
  override def toString = s"%RANGE\n$min,$max\n"
}

final case class Type(`type`: Int) {
  override def toString = s"%TYPE\n${`type`}\n"
}

final case class Enabled(enabled: Boolean) {
  override def toString = "%EN\n" + (if (enabled) "1" else "0") + "\n"
}

final case class Diagnostic(diagnostic: String) {
  override def toString = s"%DIAGSTART\n$diagnostic%DIAGEND\n"
}

case class Sensor(
  index: Int, name: Name, daqRate: Option[DaqRate],
  range: Option[Range], `type`: Option[Type], enabled: Option[Enabled],
  diagnostic: Option[Diagnostic]
) {
  override def toString =
    List(Some(s"%SENSOR$index<$name"), daqRate, range, `type`, enabled, diagnostic, Some(">")).flatten.mkString
}

final case class Actuator(
  index: Int, name: Name, range: Option[Range], `type`: Option[Type],
  enabled: Option[Enabled], diagnostic: Option[Diagnostic]
) {
  override def toString =
    List(Some(s"%ACTUATOR$index<$name"), range, `type`, enabled, diagnostic, Some(">")).flatten.mkString
}

final case class Device(devInfo: DevInfo, sensors: List[Sensor], actuators: List[Actuator]) {
  override def toString = "[" + devInfo.toString + sensors.mkString + actuators.mkString + "]"
}