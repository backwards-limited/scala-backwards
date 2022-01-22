package tech.backwards.fp.applicative

import tech.backwards.fp.writer.Writer
import org.scalatest.matchers.must.Matchers
import org.scalatest.wordspec.AnyWordSpec

class WriterSpec extends AnyWordSpec with Matchers {
  import tech.backwards.fp.applicative.WriterOps._
  import tech.backwards.fp.functor.WriterOps._
  import tech.backwards.fp.monoid.ListOps._

  "Writer Applicative" should {
    "work" in {
      val `writer fmap`: Writer[List[String], Int => Int => (Int, Int, Int)] =
        Writer(() => (List("een"), 1)) fmap ((x: Int) => (y: Int) => (z: Int) => (x, y, z))

      val `<$> writer`: Writer[List[String], Int => Int => (Int, Int, Int)] =
        ((x: Int, y: Int, z: Int) => (x, y, z)).curried `<$>` Writer(() => (List("een"), 1))

      val `<$> writer <*>`: Writer[List[String], Int => (Int, Int, Int)] =
        `<$> writer` <*> Writer(() => (List("twee"), 2))

      val `<$> writer <*> <*>`: Writer[List[String], (Int, Int, Int)] =
        `<$> writer <*>` <*> Writer(() => (List("dree"), 3))

      `<$> writer <*> <*>`.run() mustBe (List("een", "twee", "dree"), (1, 2, 3))
    }
  }
}