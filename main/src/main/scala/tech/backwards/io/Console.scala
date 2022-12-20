package tech.backwards.io

import scala.util.chaining.scalaUtilChainingOps
import cats.Show
import cats.implicits.toShow

object Console {
  object syntax {
    import scala.Console._

    implicit class Foo[A: Show](a: A) extends ConsoleSyntax(a.show) {
      def debug(x: String => String = identity): A =
        scala.Console.println(x(s)).pipe(_ => a)
    }

    implicit class ConsoleSyntax(val s: String) {
      val color: String => String => String =
        s => color => s"$color$s$RESET"

      val text: String => String =
        color(s)

      val black: String =
        text(BLACK)

      val red: String =
        text(RED)

      val green: String =
        text(GREEN)

      val yellow: String =
        text(YELLOW)

      val blue: String =
        text(BLUE)

      val magenta: String =
        text(MAGENTA)

      val cyan: String =
        text(CYAN)

      val white: String =
        text(WHITE)

      val blackBg: String =
        text(BLACK_B)

      val redBg: String =
        text(RED_B)

      val greenBg: String =
        text(GREEN_B)

      val yellowBg: String =
        text(YELLOW_B)

      val blueBg: String =
        text(BLUE_B)

      val magentaBg: String =
        text(MAGENTA_B)

      val cyanBg: String =
        text(CYAN_B)

      val whiteBg: String =
        text(WHITE_B)
    }
  }
}