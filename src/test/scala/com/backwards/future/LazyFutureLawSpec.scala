package com.backwards.future

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.{ExecutionContext, Future}
import cats.Eq
import cats.laws.discipline.MonadTests
import cats.tests.CatsSuite
import org.scalacheck.{Arbitrary, ScalacheckShapeless}

class LazyFutureLawSpec extends CatsSuite with ScalacheckShapeless {
  implicit def eqLazyFuture[A: Eq]: Eq[LazyFuture[A]] = Eq.fromUniversalEquals

  implicit def arbitraryLazyFuture[A: Arbitrary]: Arbitrary[LazyFuture[A]] = Arbitrary {
    implicitly[Arbitrary[A]].arbitrary.map(a => (_: ExecutionContext) => Future(a))
  }

  checkAll("LazyFuture.MonadLaws", MonadTests[LazyFuture].monad[Int, Int, String])
}