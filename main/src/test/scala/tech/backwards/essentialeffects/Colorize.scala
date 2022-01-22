package tech.backwards.essentialeffects

object Colorize {
  private val colors: List[String] =
    List(
      Console.WHITE,
      Console.BLACK + Console.WHITE_B,
      Console.RED,
      Console.GREEN,
      Console.YELLOW,
      Console.BLUE,
      Console.MAGENTA,
      Console.CYAN
    )

  private val numColors: Int =
    colors.size - 1

  def apply(a: Any): String =
    s"${colors(a.hashCode.abs % numColors)}$a${Console.RESET}"

  def reversed(a: Any): String =
    s"${Console.REVERSED}${apply(a)}"
}