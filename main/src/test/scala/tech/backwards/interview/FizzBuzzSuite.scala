package tech.backwards.interview

import scala.collection.parallel.CollectionConverters._
import scala.collection.parallel.ParSeq
import munit.FunSuite

/**
 * [[https://medium.com/swlh/fizzbuzz-in-scala-basic-parallel-reactive-distributed-587d188e109b FizzBuzz from simple to silly]]
 */
class FizzBuzzSuite extends FunSuite {
  val Fizz: String = "Fizz"
  val Buzz: String = "Buzz"
  val FizzBuzz: String = "FizzBuzz"

  def fizzbuzzRules(x: Int): String =
    (x % 3, x % 5) match {
      case (0, 0) => FizzBuzz
      case (0, _) => Fizz
      case (_, 0) => Buzz
      case _ => x.toString
    }

  test(s"Simple $FizzBuzz") {
    def fizzbuzz(n: Int)(rules: Int => String): LazyList[String] =
      LazyList.range(1, n + 1).map(rules)

    assertEquals(
      fizzbuzz(15)(fizzbuzzRules),
      LazyList("1", "2", Fizz, "4", Buzz, Fizz, "7", "8", Fizz, Buzz, "11", Fizz, "13", "14", FizzBuzz)
    )
  }

  test(s"Parallel $FizzBuzz") {
    def fizzbuzz(n: Int)(rules: Int => String): ParSeq[(Int, String)] =
      (1 to n).par.map(x => (x, rules(x)))

    assertEquals(
      fizzbuzz(15)(fizzbuzzRules).toList.sortBy(_._1).map(_._2),
      List("1", "2", Fizz, "4", Buzz, Fizz, "7", "8", Fizz, Buzz, "11", Fizz, "13", "14", FizzBuzz)
    )
  }
}