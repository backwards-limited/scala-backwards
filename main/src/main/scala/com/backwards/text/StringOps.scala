package com.backwards.text

import scala.language.postfixOps
import scala.util.Random

object StringOps extends StringOps

trait StringOps {
  val randomString: Int => String =
    stringLength => LazyList.continually(Random.nextPrintableChar()) take stringLength mkString

  val separate: String => Seq[String] = { str =>
    val separatorPattern = Seq(
      "\\s+", "_", "-",
      "(?<=[A-Z])(?=[A-Z][a-z])",
      "(?<=[^A-Z_-])(?=[A-Z])",
      "(?<=[A-Za-z])(?=[^A-Za-z])"
    ).mkString("|").r

    separatorPattern split str toSeq
  }

  val lowerCase: String => String =
    _.toLowerCase

  val upperCase: String => String =
    _.toUpperCase

  val titleCase: String => String =
    _.toLowerCase.capitalize

  def lowerCamel(s: String): String =
    convert(lowerCase, titleCase, "", s)

  def lowerCamel[T](c: Class[T]): String =
    lowerCamel(c.getSimpleName.takeWhile(_ != '$'))

  def upperCamel(s: String): String =
    convert(titleCase, titleCase, "", s)

  def upperCamel[T](c: Class[T]): String =
    upperCamel(c.getSimpleName.takeWhile(_ != '$'))

  def lowerSnake(s: String): String =
    lowerCase(convert(lowerCase, lowerCase, "_", s))

  def lowerSnake[T](c: Class[T]): String =
    lowerSnake(c.getSimpleName.takeWhile(_ != '$'))

  def upperSnake(s: String): String =
    lowerCase(convert(upperCase, upperCase, "_", s))

  def upperSnake[T](c: Class[T]): String =
    upperSnake(c.getSimpleName.takeWhile(_ != '$'))

  def lowerKebab(s: String): String =
    lowerCase(convert(lowerCase, lowerCase, "-", s))

  def lowerKebab[T](c: Class[T]): String =
    lowerKebab(c.getSimpleName.takeWhile(_ != '$'))

  def upperKebab(s: String): String =
    upperCase(convert(upperCase, upperCase, "-", s))

  def upperKebab[T](c: Class[T]): String =
    upperKebab(c.getSimpleName.takeWhile(_ != '$'))

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