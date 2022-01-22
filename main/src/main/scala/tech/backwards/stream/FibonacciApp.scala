package tech.backwards.stream

object FibonacciApp extends App {
  lazy val fibs: LazyList[BigInt] =
    BigInt(0) #:: BigInt(1) #:: fibs.zip(fibs.tail).map { case (x, y) => x + y }

  fibs.take(7).foreach(println)
}