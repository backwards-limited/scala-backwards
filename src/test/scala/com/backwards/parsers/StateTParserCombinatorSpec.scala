package com.backwards.parsers

import org.scalatest.matchers.must.Matchers
import org.scalatest.wordspec.AnyWordSpec
import cats.data.StateT._
import cats.data.StateT
import cats.implicits._

/**
 * [[http://blog.leifbattermann.de/2018/02/03/parsers-in-scala-built-upon-existing-abstractions/]]
 */
class StateTParserCombinatorSpec extends AnyWordSpec with Matchers {
  type Parser[A] = StateT[Option, String, A]

  "Parser" should {
    "be initialised" in {
      val p: Parser[String] = "42".pure[Parser]
      val r: Option[(String, String)] = p.run("initial")

      r mustBe Option("initial" -> "42")

      val pe: Parser[Nothing] = ().raiseError[Parser, Nothing]
      val re = pe.run("initial")

      re mustBe None
    }

    val item: Parser[Char] =
      for {
        input <- get[Option, String]
        _ <- if (input.nonEmpty) modify[Option, String](_.tail)
        else ().raiseError[Parser, Nothing]
      } yield input.head

    "parse whatever next character there is" in {
      val r: Option[(String, Char)] = item.run("initial")
      r mustBe Option("nitial" -> 'i')

      val re: Option[(String, Char)] = item.run("")
      re mustBe None
    }

    def sat(p: Char => Boolean): Parser[Char] =
      for {
        c <- item
        _ <- if (p(c)) c.pure[Parser]
             else ().raiseError[Parser, Nothing]
      } yield c

    "parse single characters that satisfy a given predicate" in {
      val p: Parser[Char] = sat(_ == 'i')
      p.run("initial") mustBe Option("nitial" -> 'i')

      val pe: Parser[Char] = sat(_ == 'z')
      pe.run("initial") mustBe None
    }

    val digit: Parser[Char] = sat(_.isDigit)

    val lower: Parser[Char] = sat(_.isLower)

    val upper: Parser[Char] = sat(_.isUpper)

    val letter: Parser[Char] = sat(_.isLetter)

    val alphaNum: Parser[Char] = sat(_.isLetterOrDigit)

    def char(c: Char): Parser[Char] = sat(_ == c)

    "creates some more primitive parsers" in {
      digit.run("initial") mustBe None
      digit.run("6initial") mustBe Option("initial" -> '6')

      char('z').run("initial") mustBe None
      char('z').run("zinitial") mustBe Option("initial" -> 'z')
    }

    def string(str: String): Parser[String] =
      str.map(char).toList.sequence.map(_.mkString)

    "for string literals can either be defined recursively or like this" in {
      string("ini").run("initial") mustBe Option("tial" -> "ini")
    }

    "need a combinator that applies a first parser and if it fails then applies a second parser. There is already such a combinator for StateT comming from SemigroupK called combineK or <+>" in {
      val p: Parser[String] = string("hi") <+> string("hello")

      p.run("hello world") mustBe Option(" world" -> "hello")
    }

    def many[A](p: Parser[A]): Parser[List[A]] =
      many1(p) <+> List.empty[A].pure[Parser]

    def many1[A](p: Parser[A]): Parser[List[A]] =
      for {
        a <- p
        as <- many(p)
      } yield a :: as

    "with the help of <+> we can define two mutually recursive parsers many and many1 that repeatedly apply parsers until they fail and return lists of results of 0 to n or 1 to n elements" in {
      val p = many(letter)
      p.run("well hello there") mustBe Option(" hello there" -> List('w', 'e', 'l', 'l'))
      p.run(" hello there") mustBe Option(" hello there" -> List())

      val p1 = many1(letter)
      p1.run("a hello there") mustBe Option(" hello there" -> List('a'))
      p1.run(" hello there") mustBe None
    }

    "providing list of parsers and try to apply each one until one succeeds, or not, semantically providing a list of alternatives" in {
      val ps =
        List[Parser[Char]](
          char('a'),
          char('b'),
          lower,
          digit
        )

      ps.foldK.run("foo") mustBe Option("oo" -> 'f')
    }

    val ident: Parser[String] =
      (lower, many(alphaNum)).mapN(_ :: _).map(_.mkString)

    val nat: Parser[Int] =
      many1(digit).map(_.mkString.toInt)

    val space: Parser[Unit] =
      many(sat(_.isWhitespace)).map(_ => ())

    // In this case the spaces around the token are discarded while the token is kept.
    def token[A](p: Parser[A]): Parser[A] =
      space *> p <* space

    val identifier: Parser[String] = token(ident)

    val natural: Parser[Int] = token(nat)

    def symbol(s: String): Parser[String] = token(string(s))

    "number" in {
      val r: Option[(String, Int)] = nat.run("105 54")
      r mustBe Option(" 54" -> 105)
    }

    lazy val expr: Parser[Int] =
      for {
        t <- term
        res <- (for {
                  _ <- symbol("+")
                  e <- expr
                } yield t + e) <+> t.pure[Parser]
      } yield res

    lazy val term: Parser[Int] =
      for {
        f <- factor
        res <- (for {
                  _ <- symbol("*")
                  t <- term
                } yield f * t) <+> f.pure[Parser]
      } yield res

    lazy val factor: Parser[Int] =
      (for {
        _ <- symbol("(")
        e <- expr
        _ <- symbol(")")
      } yield e) <+> natural

    "arithmetic" in {
      def eval(input: String): String Either Int =
        expr.run(input) match {
          case Some(("", n))  => Right(n)
          case Some((out, _)) => Left(s"unconsumed input: $out")
          case None           => Left("invalid input")
        }

      eval("2 * (3 + 4)") mustBe Right(14)

      eval("2 * 3 + 4") mustBe Right(10)

      eval("2 + 3 * 4") mustBe Right(14)
      eval("(2 + 3) * 4") mustBe Right(20)

      eval(" (( 1 )) * ( 2+ ( ( (   3) ) ) * (4 + (  ((5)) + 6)) * ( ((7 * 8   ))) +9)") mustBe Right(2531)
    }
  }
}