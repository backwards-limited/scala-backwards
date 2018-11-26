package com.backwards.future

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.{ExecutionContext, Future}
import cats.Eq
import cats.laws.discipline.MonadTests
import cats.tests.CatsSuite
import org.scalacheck.{Arbitrary, ScalacheckShapeless}

// TODO - Doesn't work as Future is not a true Monad, so did I waste my time attempting to prove the monadic laws!
class LazyFutureLawSpec extends CatsSuite with ScalacheckShapeless {
  implicit def eqLazyFuture[A: Eq]: Eq[LazyFuture[A]] = Eq.fromUniversalEquals

  implicit def arbitraryLazyFuture[A: Arbitrary]: Arbitrary[LazyFuture[A]] = Arbitrary {
    implicitly[Arbitrary[A]].arbitrary.map(a => (_: ExecutionContext) => Future.successful(a))
  }

  // checkAll("LazyFuture.MonadLaws", MonadTests[LazyFuture].monad[Int, Int, String])
}