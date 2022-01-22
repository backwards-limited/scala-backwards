package tech.backwards.dependenttypes

import org.scalatest.matchers.must.Matchers
import org.scalatest.wordspec.AnyWordSpec

/**
 * [[https://velvia.github.io/Scala-Path-Dependent-Types/ Scala Path Dependent Types]]
 */
class ComparatorSpec extends AnyWordSpec with Matchers {
  "" should {
    "" in {
      /*
      We could compare with a type class, but this requires knowing what types we will be comparing at compile time:

      trait Comparator[T] {
        def ordering: Ordering[T]

        def compare(a: T, b: T): Int = ordering.compare(a, b)
      }

      object IntComparator extends Comparator[Int] {
        def ordering: Ordering[Int] = Ordering.Int
      }
      */

      trait Comparator {
        type T // Abstract type member, to be filled in by concrete classes

        def ordering: Ordering[T]

        def compare(a: T, b: T): Int = ordering.compare(a, b)
      }

      object IntComparator extends Comparator {
        type T = Int

        def ordering: Ordering[Int] = Ordering.Int
      }

      /*
      The following will not work:

      def process[T: Comparator](items: Seq[T]): Int = ???
      <console>:11: error: Comparator does not take type parameters
        def process[T: Comparator](items: Seq[T]): Int =

      This is because the Scala syntax [T: Comparator] is really shorthand for having a second parameter list which is similar to
      (implicit c: Comparator[T])
      and Comparator no longer has a type parameter.
      */

      def process(c: Comparator)(items: Seq[c.T]): Int =
        c.compare(items(0), items(1))

      process(IntComparator)(Seq(0, 1)) mustBe -1
    }
  }
}