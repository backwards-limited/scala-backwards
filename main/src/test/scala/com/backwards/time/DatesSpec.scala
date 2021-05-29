package com.backwards.time

import java.time.LocalDate
import org.scalatest.matchers.must.Matchers
import org.scalatest.wordspec.AnyWordSpec

// TODO - WIP
class DatesSpec extends AnyWordSpec with Matchers {
  "" should {
    "" in {
      def dates(fromDate: LocalDate): LazyList[LocalDate] =
        fromDate #:: dates(fromDate plusMonths 1)

      val ds = dates(LocalDate.of(2020, 5, 24)).takeWhile(_.isBefore(LocalDate.now)).toList

      println(ds)
    }
  }
}