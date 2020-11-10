package com.backwards.fp

import cats.data.NonEmptyList
import org.scalatest.matchers.must.Matchers
import org.scalatest.wordspec.AnyWordSpec

/**
 * [[https://www.matfournier.com/2020-01-10-getting-funcey-part1-types/ Series of 5 FP articles]]
 * {{{
 * ------------------------------
 * |                            |
 * |      ----------------      |
 * |     |                |     |
 * |     |    PURE        |     |
 * |     |   FUNCTIONS    |     |
 * |     |________________|     |
 * |                            |
 * | Side-effecting functions   |
 * |____________________________|
 * outside world / program boundary
 * }}}
 *
 *  - We want as much of our business logic living in pure functions - The output is determined solely by the input which is easy to test.
 *  - We want a clean edge at the boundary of our program (our side-effecting functions that take in JSON at an endpoint, make http requests) that deal with the possibility of failures, parsing errors, etc.
 *  - We want to use restriction at this boundary so the downstream dependencies deal with nice domain models and focus on the business logic.
 *
 * Types and functions are sets! A pure function is a mapping from A => B that is every value in A has a corresponding value in B.
 * {{{
 * // pure
 *
 * input / domain              output / codomain
 *
 * --------------              ------------------
 *     a1 -----------------------> b1
 *
 *     a2 -----------------------> b2
 *
 *
 * // non-deterministic, a2 maps to two possible values
 *
 * --------------              ------------------
 *
 * input / domain              output / codomain
 *
 * --------------              ------------------
 *     a1 -----------------------> b1
 *
 *     a2 -----------------------> b2
 *                   |
 *                   |___________> b3
 *
 * For example:
 *
 * import scala.util.Random
 * Random.nextInt(100) // 28
 * Random.nextInt(100) // 17
 * }}}
 *
 * Partial functions are not pure.
 * We don't mean PartialFunction in the scala syntax way. We mean partial functions where a value in the input domain does not map to a corresponding value in the output domain. The two common cases we see for these are:
 *
 *  - exceptions
 *  - nulls
 *
 * {{{
 *  def addOne(x: Int): Int = x + 1
 *  addOne(null) // boom, scala.matchError: null
 *
 *  def div(x: Int, y: Int): Int = if (y != 0) x / y else throw new Exception("boom")
 * }}}
 *
 * Worse, we've lied in our type signatures.
 * We've thrown away the power we could have, that is, reading the type signature can tell us what's going on.
 * We have not signalled our intention that something can go wrong. Now upstream callers are forced to defensively put try {..} catch {..} everywhere and the code becomes hard to read.
 *
 * So a pure function is:
 *
 *  - deterministic
 *  - total (not partial)
 *  - has no mutation (local mutation that does not escape the function is fine)
 *  - no exceptions
 *  - no nulls
 *  - no reflection
 *  - no side-effects
 *
 * The benefit of this is that we gain referential transparency.
 *
 *
 * All the machinery of FP with the funny math words and the fact we can map some category theory to FP comes from referential transparency.
 * We gain a ton of ability to reason about our programs.
 * The type signatures act as huge markers to tell us what's going on to reduce the cognitive load when we read code.
 * The design patterns and abstractions in use all come from wanting to maintain this property.
 *
 * The catch is that a program of pure functions is pretty useless - It can't do anything.
 *
 * We know we need to talk to the outside world.
 * We know network requests fail.
 * We know there is a world of impure things we have to deal with:
 *
 *  - partiality
 *  - exceptions
 *  - non-determinism
 *
 * Contexts!
 *
 * All of these it turns out are a context. Sometimes called an effect. You can think of them as a box. The following are all a context of some kind:
 *
 *  - partiality
 *  - exceptions
 *  - non-determinism
 *  - dependency injection
 *  - logging
 *  - mutable state
 *  - IO side-effects
 *
 * These contexts are all around us and in most languages we don't think about them: they are implicit contexts.
 * But we are in a language with a rich expressive type system! We want to make these contexts explicit and put them into our type signatures.
 * The sooner you see these "busy" type signatures as friends, the easier your code will become to read.
 *
 * Effects - Just another word for context.
 *
 * So what do the likes of Option, Either, List, Task etc. have in common?
 *
 * They all compute some sort of answer with some extra stuff associated with them - the extra stuff is what we call an effect.
 *
 * They all share the same Shape F[A]:
 *
 *  - type F[A] = Option[A]
 *  - type F[A] = Either[E, A] // for any fixed E
 *  - type F[A] = List[A]
 *  - type F[A] = Reader[E, A] // for any type E
 *  - type F[A] = Writer[W, A] // for any type W
 *  - type F[A] = State[S, A]  // for any type S
 *  - type F[A] = Future[A]
 *  - type F[A] = Task[A]
 *  - type F[A] = Validation[E, A] // for any type E
 *
 * What is an effect? Whatever distinguishes F[A] from A.
 *
 *  - F[A] is sometimes called a Context
 *  - sometimes F[A] is called "a program in F that computes a value of A"
 *  - sometimes F[A] is called a computation
 *
 * The problem with an effect like Future / Task / IO is that it's vague. It's the String -> String of the effect world. What is Task[Permission] doing?
 *
 *  - hitting a DB (probably)
 *  - hitting some kafka audit service for DB access
 *  - logging
 *  - hitting some microservice Mat wrote 6 months ago that no one really knows about
 *  - mining bitcoin?
 *
 * In Haskell, instead of:
 * {{{
 *   signOauth :: Oauth -> Credential -> Request -> Request
 * }}}
 *
 * We would like to restrict the context and introduce a constraint at the type level:
 * {{{
 *  signOauth :: MonadOauth m => Oauth -> Credential -> Request -> m Request
 * }}}
 * This means that we can make this request, but only in the context of MonadOauth.
 */
class FunctionalSpec extends AnyWordSpec with Matchers {
  "When we restrict what we can do, it's easier to understand what we can do" should {
    "demo" in {
      /*
      Push the responsibility backwards (restriction):
      - we restrict the range of inputs we will take
      - instead of taking Int, we only take Natural numbers
      - instead of taking Int for http code, we take an enum of HttpCodes
      */

      // Some fn that takes in a list and concatenates all the elements but has some business logic saying that there can never be an empty string:
      import cats.implicits.toFoldableOps
      import cats.implicits.catsSyntaxOptionId

      def concatStrings(xs: List[String]): Option[String] =
        if (xs.isEmpty) None else xs.combineAll.some

      // We can improve (restrict) with the following:
      def concatStringsImprovement(xs: NonEmptyList[String]): String =
        xs.combineAll
    }
  }

  "By making something more abstract we've made it more precise - Freedom at one level leads to restriction at another" should {
    "demo" in {
      def fooi(a: Int): Int = ???

      def foos(s: String): String = ???

      def foo[A](a: A): A = identity(a)

      /*
      There is near infinite implementations of a function that would fit the first type signature.
      The second String -> String has infinite implementations.
      The last, even though it's generic, is actually the most constrained -There is only one function you can write that will work for the generic foo and that is identity.

      This is counter-intuitive! But it's the very fact we don't know what A is that gives us this property.
      */
    }
  }

  "Referential transparency" should {
    "demo" in {
      val area = (radius: Int) => math.Pi * math.pow(radius, 2)

      val program = area(3) + area(4)

      // Area is referentially transparent. If we substitute it's value in program:

      val programSub = (math.Pi * math.pow(3, 2)) + (math.Pi * math.pow(4, 2))

      program mustBe programSub
    }
  }
}