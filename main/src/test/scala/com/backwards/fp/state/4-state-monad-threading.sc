// State management - FP
import cats.data._
import cats.implicits._

/*
(->> (range)
     (map #(* 10 %))
     (take 5))
*/

val x = State[Int, String](s =>
  s + 1 -> s"$s"
)

x.run(1).value

// 1
// 2, 1
// 3, 2
val xTwice = x *> x

xTwice.run(1).value

val xTwiceFM = x flatMap(str =>
  x
)

xTwiceFM.run(1).value

val fc: State[Int, String] = for {
  a <- x
  b <- x
} yield
  a

fc.run(1).value