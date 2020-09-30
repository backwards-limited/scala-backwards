package com.backwards.fp

import org.scalatest.matchers.must.Matchers
import org.scalatest.wordspec.AnyWordSpec

class HigherKindedTypeSpec extends AnyWordSpec with Matchers {
  "Typeclasses constrain higher-kinded types elegantly" should {
    "show example with Bind" in {
      // To constrain F to types with the flatMap and map we need, we use typeclasses.

      trait Bind[F[_]] {
        def map[A, B](fa: F[A])(f: A => B): F[B]

        def flatMap[A, B](fa: F[A])(f: A => F[B]): F[B]
      }

      // Then we define instances for the types we’d like to have this on: Bind[List], Bind[Option], and so on.

      def tuple[F[_], A, B](fa: F[A], fb: F[B])(implicit F: Bind[F]): F[(A, B)] =
        F.flatMap(fa) { a =>
          F.map(fb)(a -> _)
        }
    }
  }
}