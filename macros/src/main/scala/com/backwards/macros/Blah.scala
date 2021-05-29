/*
package com.backwards.macros

import scala.annotation.StaticAnnotation
import scala.language.experimental.macros
import scala.reflect.macros.whitebox


object Blah {
  implicit def materializeBlah: Product = macro materializeBlahImpl

  def materializeBlahImpl(c: whitebox.Context)/*(annottees: c.Expr[Any]*)*/ = {
    import c.universe._



    /*val className = annottees.map(_.tree) match {
      case List(q"class $name") => name
      case _ => c.abort(c.enclosingPosition, "the annotation can only be used with classes")
    }*/

    /*annottees match {
      // @ADT trait Foo { ... }
      case List(Expr(cls: ClassDef)) =>
        val ClassDef(clsMods, clsName, clsParams, clsTemplate) = cls

        q"""
          case class $clsName() {
              val blah = "blah blah"
            }
        """

      // @ADT trait Foo { ... }; object Foo { ... }
      //case List(Expr(cls: ClassDef), Expr(obj: ModuleDef)) => runClassWithObj(cls, obj)
      //case List(Expr(obj: ModuleDef), Expr(cls: ClassDef)) => runClassWithObj(cls, obj)

      case _ => c.abort(c.enclosingPosition, s"Invalid @FromSchema usage")
    }*/

    /*c.Expr[Any](
      q"""
          case class $className() {
              val blah = "blah blah"
            }
        """)*/

    q"""
      package com.backwards.auto {

        case class Blam() {
            val blah = "blah blah"
          }
      }
    """
  }
}

/*
@FromSchema("")
class Foo*/
*/
