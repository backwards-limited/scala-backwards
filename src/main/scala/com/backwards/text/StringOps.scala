package com.backwards.text

import scala.language.postfixOps
import scala.util.Random

object StringOps extends StringOps

trait StringOps {
  val randomString: Int => String =
    stringLength => Stream.continually(Random.nextPrintableChar) take stringLength mkString

  val lowerCase: String => String =
    _.toLowerCase

  val upperCase: String => String =
    _.toUpperCase

  val titleCase: String => String =
    _.toLowerCase.capitalize

  val lowerCamel: String => String =
    convert(lowerCase, titleCase, "", _)

  val upperCamel: String => String =
    convert(titleCase, titleCase, "", _)

  val lowerSnake: String => String =
    convert(lowerCase, lowerCase, "_", _)

  val upperSnake: String => String =
    convert(upperCase, upperCase, "_", _)

  val lowerKebab: String => String =
    convert(lowerCase, lowerCase, "-", _)

  val upperKebab: String => String =
    convert(upperCase, upperCase, "-", _)

  val separate: String => Seq[String] = { str =>
    val separatorPattern = Seq(
      "\\s+", "_", "-",
      "(?<=[A-Z])(?=[A-Z][a-z])",
      "(?<=[^A-Z_-])(?=[A-Z])",
      "(?<=[A-Za-z])(?=[^A-Za-z])"
    ).mkString("|").r

    separatorPattern split str toSeq
  }

  def convert(headTransform: String => String, tailTransform: String => String, sep: String, str: String): String =
    (separate(str) match {
      case head :: tail => headTransform(head) :: tail.map(tailTransform)
      case list => list
    }) mkString sep

  def randomIp: String = {
    val random: Int => Int =
      Random.nextInt

    s"${random(223) + 1}.${(1 to 3).map(_ => random(255)) mkString "."}"
  }
}