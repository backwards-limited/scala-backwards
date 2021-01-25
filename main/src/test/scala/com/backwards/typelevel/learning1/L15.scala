package com.backwards.typelevel.learning1

import shapeless._
import shapeless.labelled._
import shapeless.syntax.singleton._

/**
 * Literal and "singleton" types
 */
object L15 extends App {

  val x: Witness.`42`.T = 42.narrow
  // val x: 42 = 42.narrow <- Nicer with 2.13 but we'll leave as is for illustrative reasons

  // `42` behaves as regular Int
  val y: Int = x + 1

  trait HttpPort

  def initPortWithPhantomType(port: Int with HttpPort): Unit = {}

  val number = 80
  val portWithPhantomType = number.asInstanceOf[Int with HttpPort]

  initPortWithPhantomType(portWithPhantomType)
  // Pure Int can't be provided
  // initPortWithPhantomType(80)

  val portWithTag = Symbol("port") ->> 80

  /*def getFieldName[K: Witness.Aux, V](value: FieldType[K, V]): K =
    implicitly[Witness.Aux[K]].value*/

  def getFieldName[K, V](value: FieldType[K, V])(implicit witness: Witness.Aux[K]): K =
    witness.value

  println(getFieldName(portWithTag))
  // println(getFieldName(80))

  def getFieldValue[K, V](value: FieldType[K, V]): V =
    value

  println(getFieldValue(portWithTag))
  // println(getFieldValue(80))

  def initPortWithTag[K, V](port: FieldType[K, V]): Unit = {}

  initPortWithTag(portWithTag)
  // Pure Int can't be provided
  // initPortWithTag(80)
}