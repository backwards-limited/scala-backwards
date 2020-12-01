package com.backwards.categorytheory

import java.util.concurrent.TimeUnit
import scala.collection.mutable
import scala.util.Random
import scala.util.chaining.scalaUtilChainingOps
import org.scalatest.matchers.must.Matchers
import org.scalatest.wordspec.AnyWordSpec

class Chap2Spec extends AnyWordSpec with Matchers {
  "Chap 2" should {
    "memoize" in {
      def memoize[A, B](f: A => B): A => B = {
        println(s"Memoize function $f")
        val memoized: mutable.Map[A, B] = mutable.Map.empty

        def call(f: => B): B = {
          println("Calling your function")
          f
        }

        a: A => memoized.getOrElseUpdate(a, call(f(a)))
      }

      val myFn: Int => String =
        i => TimeUnit.SECONDS.sleep(5).pipe(_ => i.toString)

      val myFnMemoized = memoize(myFn)

      myFnMemoized(5) mustBe "5"
      println("Done")

      myFnMemoized(5) mustBe "5"
      println("Done")


      val random: Any => Int = {
        println("Random initialised")
        val r = new Random(42)

        _ => r.nextInt
      }

      val randomMemoized = memoize(random)

      println(randomMemoized())
      println(randomMemoized())
    }
  }
}