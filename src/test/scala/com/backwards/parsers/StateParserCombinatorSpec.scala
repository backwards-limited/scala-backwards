package com.backwards.parsers

import cats.data.{State, StateT}
import cats.implicits.{catsSyntaxApplicativeErrorId, catsSyntaxApply, catsSyntaxApplyOps, catsSyntaxTuple4Semigroupal}
import cats.{ApplicativeError, Eval}
import org.scalatest.matchers.must.Matchers
import org.scalatest.wordspec.AnyWordSpec

/**
 * [[https://www.sderosiaux.com/articles/2018/06/15/a-simple-way-to-write-parsers-using-the-state-monad/]]
 */
class StateParserCombinatorSpec extends AnyWordSpec with Matchers {
  /**
   * A parser can be seen as a function: String => Option[A].
   * We give it an input, it outputs a value or not (a parser is not total: when it can’t match its input, then we have an error or a fallback).
   * It’s easy to combine such parsers using PartialFunction’s orElse.
   */
  "Parser" should {
    "can be thought of as a partial function" in {
      val x: PartialFunction[String, Int] = { case s if s.matches("[0-9]+") => s.toInt }
      val y: PartialFunction[String, Int] = PartialFunction.fromFunction(_.length)
      val xy = x orElse y

      a [MatchError] must be thrownBy x("toto")
      xy("123") mustBe 123
      xy("toto") mustBe 4
    }

    "be a partial function" in {
      type Parser[A] = String => Option[A]

      val IntRegex = "([0-9]+)".r

      val intParser: Parser[Int] = {
        case IntRegex(i) => Option(i.toInt)
        case _ => None
      }

      intParser("123") mustBe Option(123)
      intParser("toto") mustBe None
    }
  }

  /**
   * State is a structure that provides a functional approach to handling application state.
   * State[S, A] is basically a function S => (S, A), where S is the type that represents your state and A is the result the function produces.
   * In addition to returning the result of type A, the function returns a new S value, which is the updated state.
   */
  "State Monad" should {
    "be introduced" in {
      val c = State[String, Char](s => (s.tail, s.head))

      c.run("re").value  mustBe ("e", 'r')  // i.e. res0: (String, Char) = (e,r)
      c.run("are").value mustBe ("re", 'a') // i.e. res0: (String, Char) = (re,a)
    }

    "be more than just a function (as shown in 'introduced') - State is also a monad" in {
      val head = State[String, Char](s => (s.tail, s.head))
      val toInt = State[String, Int](s => ("", s.toInt))

      val product: State[String, Int] = for {
        h <- head
        i <- toInt
      } yield h * i

      val initialState = "23"
      product.run(initialState).value mustBe ("", 150) // '2' * 3 = 50 * 3 = 150

      /*
      It also encapsulates the value into an effect! This is why we used product.run(...).value.
      We need to add .value because State is an alias for StateT[Eval, S, A]:
      therefore it returns an Eval (an Eval is a monad containing a computation or value, strict or lazy).
      */
    }

    "be better than just actual functions" in {
      val head: String => (String, Char) = s => (s.tail, s.head)
      val toInt: String => (String, Int) = s => ("", s.toInt)

      val product: String => (String, Int) = s => {
        val (state, ch) = head(s)
        val (state2, i) = toInt(state)
        (state2, ch * i)
      }

      val initialState = "23"
      product(initialState) mustBe ("", 150) // ('2' * 3 = 50 * 3)
    }

    "modify" in {
      val toInt = State[String, Int](s => ("", s.toInt))
      val tail: State[String, Unit] = State.modify[String](_.tail)

      val product: State[String, Int] = for {
        _ <- tail // We ignore the value, it's ()
        i <- toInt
      } yield i

      product.run("23").value mustBe ("", 3)
    }

    "highlight that working with State and for-comprehension (flatMap), to remember 2 things" in {
      /*
      - the new state is automatically given to the next State, it’s not magic (it’s part of the flatMap implementation of State)

      - according to the inner monad of StateT (Eval for State but could be anything like Option),
        it’s possible to short-circuit the rest of the for-comprehension:

        None.flatMap(x => ???) is still None, it does nothing
        same for Left("boom").flatMap(x => ???)
      */
    }
  }

  "State to Parser" should {
    "step 1 - initially State may throw an exception" in {
      "".tail mustBe ""
      a [NoSuchElementException] must be thrownBy "".head

      val c = State[String, Char](s => (s.tail, s.head))
      a [NoSuchElementException] must be thrownBy c.run("").value
    }

    "step 2 - look into error handling" in {
      val c: State[String, Char] = for {
        state <- State.get[String]        // Alias for: State(s => (s, s)), we want the current state
        _ <- State.modify[String](_.tail) // We alter the State's state
      } yield state.head                  // The value of the State BUT will bomb out if empty i.e. ""

      c.run("") mustBe an [Eval[_]]

      a [NoSuchElementException] must be thrownBy c.run("").value

      /*
      It would be nice to add a condition to check if the state is empty before processing:

      val c: State[String, Char] = for {
        state <- State.get[String]
        _ <- if (state.isEmpty)
              // ???
              // We need to short-circuit here to not evaluate yield state.head, how?
              // ().raiseError[State[String, *], String] // ERR: no ApplicativeError instance available :(
             else
              State.modify[String](_.tail)
        } yield state.head
      */

      /*
      It should be possible to short-circuit flatmapping if Eval (the monad inside State) is itself short-circuitable. Is it?
      To be so, the monad needs to be a MonadError. It doesn’t look like Eval is. ;(
      */

      /*
      implicit val stateAE = new ApplicativeError[State[String, *], Unit] {
        override def raiseError[A](e: Unit): State[String, A] =
          State[String, A](s => ("", ???))
          // We can't construct any State because we can't construct any A!
      }

      ().raiseError[State[String, ?], Unit].run("hey").value
      // Exception scala.NotImplementedError

      All this is because State depends upon Eval, which is not a MonadError as we said. It can’t represent an error state.
      We need to change our inner monad!
      */
    }

    "step 3 - swap out Eval for Option" in {
      type Parser[A] = StateT[Option, String, A]

      val c: Parser[Char] = for {
        state <- StateT.get[Option, String]
        _ <- if (state.isEmpty) ().raiseError[Parser, String]
             else StateT.modify[Option, String](_.tail)
      } yield state.head

      c.run("toto") mustBe Option("oto" -> 't')

      c.run("") mustBe None

      /*
      Option has the following associated behaviour (decoupled because of type classes):

      trait OptionInstances extends cats.kernel.instances.OptionInstances {
        implicit val catsStdInstancesForOption: Traverse[Option] with MonadError[Option, Unit] .....
          def raiseError[A](e: Unit): Option[A] = None
      */
    }

    "step 4a - can rewrite step 3" in {
      type Parser[A] = StateT[Option, String, A]

      val c: Parser[Char] = for {
        state <- StateT.get[Option, String]
        _ <- if (state.isEmpty) ApplicativeError[Parser, Unit].raiseError(())
             else StateT.modify[Option, String](_.tail)
      } yield state.head

      c.run("toto") mustBe Option("oto" -> 't')

      c.run("") mustBe None
    }

    "step 4b - can rewrite step 3" in {
      type Parser[A] = StateT[Option, String, A]

      val c: Parser[Char] = for {
        state <- StateT.get[Option, String]
        _ <- if (state.isEmpty) StateT.liftF[Option, String, Char](None)
             else StateT.modify[Option, String](_.tail)
      } yield state.head

      c.run("toto") mustBe Option("oto" -> 't')

      c.run("") mustBe None
    }
  }

  "StateT arithmetic Parser" should {
    "" in {
      type Parser[A] = StateT[Option, String, A]

      val c: Parser[Char] = for {
        state <- StateT.get[Option, String]
        _ <- if (state.isEmpty) ApplicativeError[Parser, Unit].raiseError(())
        else StateT.modify[Option, String](_.tail)
      } yield state.head

      def matchChar(p: Char => Boolean): Parser[Char] = for {
        char <- c
        _ <- if (p(char))
          StateT.get[Option, String] // We don't use it, but we need to return a StateT!
        else
          ().raiseError[Parser, Unit]
      } yield char

      // OR
      def matchCharAlt(p: Char => Boolean): Parser[Char] =
        c.flatMapF(c => Option.when(p(c))(c))

      def digit: Parser[Char] = matchChar(_.isDigit)
      def letter: Parser[Char] = matchChar(_.isLetter)
      def space: Parser[Char] = matchChar(_.isWhitespace)

      // We can create a parser that matches a specific character
      def customChar(c: Char): Parser[Char] = matchChar(_ == c)

      digit.run("01") mustBe Option("1" -> '0')
      digit.run("ab") mustBe None

      letter.run("ab") mustBe Option("b" -> 'a')

      // Alt
      matchCharAlt(_.isDigit).run("01") mustBe Option("1" -> '0')
      matchCharAlt(_.isDigit).run("ab") mustBe None

      matchCharAlt(_.isLetter).run("ab") mustBe Option("b" -> 'a')

      // More than one character parsers:
      def string(word: String): Parser[String] = StateT[Option, String, String](s =>
        if (s.startsWith(word))
          Option(s.drop(word.length), word)
        else
          None
      )

      import cats.implicits.toTraverseOps

      // We can also reuse "customChar" to deal with errors (non-match)
      def string2(s: String): Parser[String] = s.map(customChar).toList.sequence.map(_.mkString)

      def number: Parser[String] = StateT[Option, String, String](s =>
        if (s.headOption.exists(_.isDigit)) // Dealing with empty string
          Option(s.partition(_.isDigit).swap)
        else
          None
      )

      string("toto").run("toto") mustBe Option("", "toto")
      string("toto").run("toto-ahah") mustBe Option("-ahah", "toto")

      string("toto").run("titi") mustBe None

      number.run("123to") mustBe Option("to" -> "123")

      /*
      Combination using Applicatives

      map2[A, B, Z](fa: F[A], fb: F[B])(f: (A, B) => Z): F[Z] to combine two of them and chose what to do

      forEffect alias <* alias productL: discard the right value

      followedBy alias *> alias productR: discard the left value

      (...).mapN((A, B, C, ...) => Z): syntax sugar available on any tuples
      */

      val s1 = string("toto") *> string("titi")
      s1.run("tototiti") mustBe Option("" -> "titi")              // Has consumed "toto" then "titi"; kept the second value

      val s2 = string("toto") <* string("titi")
      s2.run("tototiti") mustBe Option("" -> "toto")              // Has consumed "toto" then "titi"; kept the first value

      val s3 = string("toto") <* string("titi") *> string("a")
      s3.run("tototitia") mustBe Option("" -> "toto")             // Consumed everything; kept the first

      val s4 = (string("toto") <* string("titi")) *> string("a")  // Consumed everything; kept the last
      s4.run("tototitia") mustBe Option("" -> "a")

      val s5 = string("toto") *> string("titi") <* string("a")    // Consumed everything; kept the middle
      s5.run("tototitia") mustBe Option("" -> "titi")

      // It’s sometimes clearer to combine Applicative’s using mapN:

      val array = (string("toto"), string("["), number, string("]")).mapN((a, b, c, d) => (a, c))
      array.run("toto[42]") mustBe Option("" -> ("toto" -> "42"))

      /*
      In the same way we previously used orElse to “fallback” on another PartialFunction when the first one couldn’t handle the input,
      it’s possible to provide a fallback to our parsers (StateT).
      This is called backtracking: the 1st parser fails to match, then the 2nd parser starts from the same beginning.
      */
      import cats.implicits.toSemigroupKOps

      val combineKExample = string("A") <+> string("B")

      combineKExample.run("Axxx") mustBe Option("xxx" -> "A")
      combineKExample.run("Bxxx") mustBe Option("xxx" -> "B")
      combineKExample.run("C") mustBe None

      /*
      <+> is syntax sugar coming from SemigroupK.
      It is defined as def <+>(y : F[A]) : F[A].
      It’s just a synonym of combineK: combining two F[A]s to get an F[A]. (Semigroup combines two A only)

      List is a ubiquitous example:
      */

      List(1) <+> List(2, 3) mustBe List(1, 2, 3)

      // Without using the SemigroupK thing, we could also configure a fallback using Option.orElse (because our F is Option, that’s not always the case!):
      val aOrB = StateT[Option, String, String](s => string("A").run(s) orElse string("B").run(s))
      // If the parsing for "A" fails (it will raiseError, ie: return None), then it will try with the other one.

      aOrB.run("B") mustBe Option("" -> "B")

      // Let’s say we want to parse multiple digits without our custom number parser. We know how to match a digit, so why not reuse this?
      val oneDigit = digit.map(_.toInt)
      val twoDigits = digit.flatMap(a => digit.map(b => s"$a$b".toInt))
      val threeDigits = digit.flatMap(a => digit.flatMap(b => digit.map(c => s"$a$b$c".toInt)))

      threeDigits.run("123") mustBe Option("" -> 123)
      threeDigits.run("12f") mustBe None

      // Remember
      None <+> Option("hi") mustBe Option("hi")
      Option("hi") <+> None mustBe Option("hi")

      val atMostTwoDigits = for {
        a <- digit
        b <- digit.map(List(_)) <+> StateT.pure(List()) // Avoids a short-circuit
      } yield s"$a${b.headOption.getOrElse("")}".toInt

      atMostTwoDigits.run("1") mustBe Option("" -> 1)

      atMostTwoDigits.run("123") mustBe Option("3" -> 12)

      // Clearly, something is recursive around here. Let’s make a function we’ll call while we find digits:
      val digitsFirstAttempt: StateT[Option, String, Int] = {
        def digits: StateT[Option, String, List[Char]] = for {
          head <- digit
          tail <- digits <+> StateT.pure(List())
        } yield head :: tail

        digits.map(_.mkString.toInt)
      }

      // And we can make the above generic to handle any type A and not just Int:
      def many[A](s: Parser[A]): Parser[List[A]] = for {
        head <- s
        tail <- many(s) <+> StateT.pure(List())
      } yield head :: tail

      val manyDigits: Parser[Int] = many(digit).map(_.mkString.toInt)

      manyDigits.run("123a") mustBe Option("a" -> 123)

      // We may want to provide a “many or none” parser (like spaces, to ignore them between tokens):
      def manyOrNone[A](s: Parser[A]): Parser[List[A]] =
        many(s) <+> StateT.pure(List())

      val spaces: Parser[Unit] =
        manyOrNone(space).map(_ => ())

      import cats.implicits.catsSyntaxTuple3Semigroupal

      def ignoreSpaces[A](s: Parser[A]): Parser[A] =
        (spaces, s, spaces).mapN((_, v, _) => v)

      // And we make our digits parser to ignore space between and after numbers
      val digits: Parser[Int] = ignoreSpaces(many(digit).map(_.mkString.toInt))

      spaces.run("   a")    mustBe Option("a" -> ())
      digits.run("1a")      mustBe Option("a" -> 1)
      digits.run("12 ")     mustBe Option("" -> 12)
      digits.run("  123 ")  mustBe Option("" -> 123)

      // Again, we can combine them:
      (digits *> digits).run(" 123 456  ") mustBe Option("" -> 456)

      // :digits: "+" :digits:
      val plusFirstAttempt: Parser[Int] = (digits, customChar('+'), digits).mapN((lhs, _, rhs) => lhs + rhs)

      plusFirstAttempt.run("1 + 5") mustBe Option("" -> 6)
      // Aha! We have our first operator

      // We can generalize our plus with some recursion, to handle any amount of +:
      // expr := digits [ '+' expr ]

      def parseOrElse[A](s: Parser[A], default: A): Parser[A] = s <+> StateT.pure(default)

      import cats.implicits.catsSyntaxTuple2Semigroupal

      def plus(lhs: Int): Parser[Int] =
        (customChar('+'), exprFirstAttempt).mapN((_, rhs) => lhs + rhs)

      def exprFirstAttempt: Parser[Int] = for {
        lhs <- digits
        res <- parseOrElse(plus(lhs), lhs) // If we can't match "+ digits", we return the lhs only
      } yield res

      exprFirstAttempt.run("1+5+ 8 + 13") mustBe Option("" -> 27)

      // Prioritize the multiplications first: we want to finish the * recursion first, to compute the value, before getting back to the other parsers (+):

      // - expr := product [ '+' expr ]
      // - product := parens [ '*' product ]
      // - parens := '(' + expr + ')' | digits

      import cats.implicits.catsSyntaxFlatMapOps

      def expr: Parser[Int] = for {
        lhs <- product
        res <- parseOrElse((customChar('+'), expr).mapN((_, rhs) => lhs + rhs), lhs)
      } yield res

      // the parser: '(' expr ')' => expr (">>" is `flatMap` ignoring the lhs value)
      def parens: Parser[Int] = (customChar('(') >> expr <* customChar(')')) <+> digits

      def product: Parser[Int] = for {
        lhs <- parens
        res <- parseOrElse((customChar('*'), product).mapN((_, rhs) => lhs * rhs), lhs)
      } yield res

      expr.runA("1+2+3")      mustBe Option(6)
      expr.runA("2*5")        mustBe Option(10)
      expr.runA("2*5*2+8")    mustBe Option(28)
      expr.runA("2*5+8*2*2")  mustBe Option(42)
    }
  }
}