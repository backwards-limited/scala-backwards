// State management - FP
import cats.data._
import cats.implicits._

// Previously we wanted to translate the following to be more FP:
val inc: (Int, Int) => (Int, Int) =
  (s, i) => (s + 1, i + 1)

val times2: (Int, Int) => (Int, Int) =
  (s, i) => (s + 1, i * 2)

val (s1, i1) = inc(0, 10)

val (s2, i2) = times2(s1, i1)

val (s3, i3) = times2(s2, i2)


// Since we can use State Monad in for comprehension we have:
def calculate(i: Int) = {
  val inc = (i: Int) => State[Int, Int](s =>
    (s + 1, i + 1)
  )

  val times2 = (i: Int) => State[Int, Int](s =>
    (s + 1, i * 2)
  )

  for {
    x <- inc(i)
    y <- times2(x)
    z <- times2(y)
  } yield z
}

calculate(10).run(0).value