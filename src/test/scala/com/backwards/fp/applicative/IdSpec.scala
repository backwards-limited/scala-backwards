package com.backwards.fp.applicative

import org.scalatest.{MustMatchers, WordSpec}
import com.backwards.fp.Id

class IdSpec extends WordSpec with MustMatchers {
  import com.backwards.fp.functor.FunctorOps._
  import com.backwards.fp.functor.IdOps._
  import com.backwards.fp.applicative.ApplicativeOps._
  import com.backwards.fp.applicative.IdOps._

  "Id Applicative" should {
    "work" in {
      /*
      1) No existing "fmap" on Id
      2) com.backwards.fp.functor.FunctorOps._ brings in toFunctorOps
         This will "wrap" so long as there is an Id Functor instance
      3) com.backwards.fp.functor.IdOps._ brings in an Id Functor instance
      4) Id is wrapped into FunctorOps and we call "fmap" with the provided function
      5) This "fmap" calls the brought in Id Functor's "fmap" where:
         def fmap[A, B](fa: Id[A])(f: A => B): Id[B] = Id(f(fa.value))
      */
      Id(1) fmap { x: Int => x + 1 } mustBe Id(2)

      /*
      Let's switch the function and type constructor around
      1) No existing "<$>" on function
      2) com.backwards.fp.functor.FunctorOps._ brings in toFunctionFunctorOps
      3) Function is wrapped into FunctionFunctorOps and we call "<$>" with the provided Id
      4) The call to "<$>" looks for an Id Functor which is provided via com.backwards.fp.functor.IdOps._
      5) This "<$>" calls the brought in Id Functor's "fmap" where:
         def fmap[A, B](fa: Id[A])(f: A => B): Id[B] = Id(f(fa.value))
      */
      { x: Int => x + 1 } `<$>` Id(1) mustBe Id(2)


      val foo: Int => Int => Int => Int =
        x => y => z => x + y + z

      /*
      Just as above, "<$>" calls "fmap" on the available Id Functor where:
      def fmap[A, B](fa: Id[A])(f: A => B): Id[B] = Id(f(fa.value))

      However, the function "foo" has multiple arguments (unlike the usual Functor examples of 1 argument).
      But "B" can be anything, and since we are essentially "currying", the B will be another function with one less argument.
      We call "fmap" as:
      fmap(Id(10)(foo) => Id[Int => Int => Int]

      since the implementation is:
      Id(foo(10)) = Id(y => z => 10 + y + z)
      */
      val `function curried via fmap`: Id[Int => Int => Int] = foo `<$>` Id(10)

      /*
      Now we have a function within a type constructor.
      We cannot repeat the above since we can only "lift" a function into a FunctionFunctorOps
      NOT a function within a higher kind i.e. the following will not compile:
      */
      "`function curried via fmap`g `<$>` Id(20)" mustNot compile

      /*
      1) No existing "<*>" on F[function]
      2) com.backwards.fp.applicative.ApplicativeOps._ brings in toApplicativeOps
         This will "wrap" so long as there is an Id Applicative instance
      3) com.backwards.fp.applicative.IdOps._ brings in an Id Applicative instance
      4) Id[function] is wrapped into ApplicativeOps ans we call the "<*>" with the provided Id
      5) The call to "<*>" calls the brought in Id Applicative's "<*>" where:
         def <*>[A, R](f: Id[A => R])(fa: Id[A]): Id[R] = Id(f.value(fa.value))
      */
      val `function curried via app`: Id[Int => Int] = `function curried via fmap` <*> Id(20)

      /*
      We continuing app(lying) until currying is complete and we have a result.
      */
      val result: Id[Int] = `function curried via app` <*> Id(30)

      result mustBe Id(10 + 20 + 30)

      /*
      Finally, altogther
      */
      ((foo `<$>` Id(10)) <*> Id(20)) <*> Id(30) mustBe Id(10 + 20 + 30)

      /*
      Note that we don't need the extra brackets, as the following shows, but IntelliJ incorrectly marks up in red:
      foo `<$>` Id(10) <*> Id(20) <*> Id(30)
      */
    }
  }
}