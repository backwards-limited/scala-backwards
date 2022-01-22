/*
package tech.backwards.macros

import scala.annotation.StaticAnnotation
//import scala.language.experimental.macros
import scala.reflect.macros.whitebox
import better.files._

import scala.meta._

class blah extends scala.annotation.StaticAnnotation {
  /*inline */def apply(defn: Any): Any = meta {
    //val q"object $name { ..$stats }" = defn

    /*val main = q"""
      def main(args: Array[String]): Unit = { ..$stats }
    """*/

    q"""
        case class Blam() {
              val blah = "blah blah"
            }
        """
  }
}*/
