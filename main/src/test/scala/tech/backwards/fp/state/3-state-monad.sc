// State management - FP
import cats.data._
import cats.implicits._

val inc = (i: Int) => State[Int, Int](s =>
  (s + 1, i + 1)
)

val times2 = (i: Int) => State[Int, Int](s =>
  (s + 1, i * 2)
)

(inc(10) flatMap times2 flatMap times2).run(0).value