package com.backwards.fp

import org.scalatest.matchers.must.Matchers
import org.scalatest.wordspec.AnyWordSpec

/**
 * [[https://www.matfournier.com/2020-01-10-getting-funcey-part1-types/ Series of 5 FP articles]]
 *
 * [[https://www.matfournier.com/2020-01-15-getting-funcey-part3-typeclasses/ Part 3]]
 *
 * A typeclass in Scala is a trait that:
 *
 *  - holds no state (duh, it's a trait)
 *  - has a type parameter (can be a higher-kinded type parameter too)
 *  - has at least one abstract method
 *  - may contain generalized methods
 *  - may extend other typeclasses
 */
class Functional3Spec extends AnyWordSpec with Matchers {
  "FizzBuzz by type class" should {
    "be implemented utilising the Monoid type class" in {
      val fizzbuzz: Int => String = {
        import cats.Monoid

        val fizz: Int => Option[String] =
          x => if (x % 3 == 0) Some("fizz") else None

        val buzz: Int => Option[String] =
          x => if (x % 5 == 0) Some("buzz") else None

        val funcs = List(fizz, buzz)

        val fizzbuzz = Monoid.combineAll(funcs)

        i => fizzbuzz(i) getOrElse i.toString
      }

      val results: List[String] = (1 to 15).toList map fizzbuzz

      println(results mkString ", ")
    }

    "be implemented utilising a custom made Monoid" in {
      trait Monoid[A] {
        def empty: A // It has an identity

        def combine(x: A, y: A): A // It has an associative combine operation
      }

      object Monoid {
        def apply[A: Monoid]: Monoid[A] = implicitly

        implicit val stringMonoid: Monoid[String] = new Monoid[String] {
          val empty = ""

          def combine(x: String, y: String): String = x + y
        }

        implicit val intMonoid: Monoid[Int] = new Monoid[Int] {
          val empty = 0

          def combine(x: Int, y: Int): Int = x + y
        }

        implicit def listMonoid[A]: Monoid[List[A]] = new Monoid[List[A]] {
          val empty: List[A] = Nil

          def combine(x: List[A], y: List[A]): List[A] = x ++ y
        }

        implicit def optionMonoid[A: Monoid]: Monoid[Option[A]] =
          new Monoid[Option[A]] {
            val empty: Option[A] = None

            def combine(x: Option[A], y: Option[A]): Option[A] = (x, y) match {
              case (None, None)         => None
              case (x @ Some(_), None)  => x
              case (None, y @ Some(_))  => y
              case (Some(x), Some(y))   => Some(Monoid[A].combine(x, y)) // use the A monoid to combine two A's
            }
          }

        /**
         * If I have a function A => B and B has a Monoid, then I have a monoid for any function A => B:
         */
        implicit def functionMonoid[A, B: Monoid]: Monoid[A => B] =
          new Monoid[A => B] {
            def empty: A => B = _ => Monoid[B].empty

            def combine(x: A => B, y: A => B): A => B = { a =>
              Monoid[B].combine(x(a), y(a))
            }
          }

        /**
         * There is a natural association with folding and lists of monoids as monoids define everything you need in a fold:
         *
         *  - the initial value and
         *  - a way to combine values.
         *
         * We can write a generic version of fold that works on monoids as follows:
         */
        implicit def foldRight[A: Monoid](as: List[A]): A =
          as.foldRight(Monoid[A].empty)(Monoid[A].combine)
      }

      val fizz: Int => Option[String] = x => if(x % 3 == 0) Some("fizz") else None
      val buzz: Int => Option[String] = x => if(x % 5 == 0) Some("buzz") else None

      val funcs: List[Int => Option[String]] = List(fizz, buzz)

      // We can combine our functions.
      // This works because we can find an option monoid for strings since we have an option monoid for strings,
      // then we can find a monoid for Int => Option[String]

      val fizzbuzz: Int => Option[String] = Monoid.foldRight(funcs)

      // Handle the None such
      val fbbOrInt: Int => String = { i =>
        fizzbuzz(i).getOrElse(i.toString) + ", "
      }

      // Map our function on a list
      val strings: List[String] = (1 to 15).toList map fbbOrInt
      println(Monoid.foldRight(strings)) // concatenate into a single string
    }
  }
}