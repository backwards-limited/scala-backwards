package com.backwards.parsers

import scala.util.matching.Regex
import scala.util.parsing.combinator._
import scala.util.parsing.input.{NoPosition, Position, Reader}
import org.scalatest.matchers.must.Matchers
import org.scalatest.wordspec.AnyWordSpec

/**
 * [[https://enear.github.io/2016/03/31/parser-combinators/ Building a lexer and parser with Scala's Parser Combinators]]
 *
 * We will be going through the process of building a lexer, to scan text into a sequence of tokens,
 * and a parser, to parse said tokens into an abstract syntax tree (AST).
 *
 * {{{
 *  read input name, country
 *    switch:
 *      country == "PT" ->
 *        call service "A"
 *        exit
 *      otherwise ->
 *        call service "B"
 *        switch:
 *          name == "unknown" ->
 *            exit
 *          otherwise ->
 *            call service "C"
 *            exit
 * }}}
 *
 * We want this to be parsed into:
 *
 * {{{
 *  AndThen(
 *    ReadInput(List(name, country)),
 *    Choice(List(
 *      IfThen(
 *        Equals(country, PT),
 *        AndThen(CallService(A), Exit)
 *      ),
 *      OtherwiseThen(
 *        AndThen(
 *          CallService(B),
 *          Choice(List(
 *            IfThen(Equals(name, unknown), Exit),
 *            OtherwiseThen(AndThen(CallService(C), Exit))
 *          ))
 *        )
 *      )
 *    ))
 *  )
 * }}}
 *
 * A possible BNF representation for this grammar is outlined below.
 *
 * {{{
 *  <block> ::= (<statement>)+
 *
 *  <statement> ::= "exit"
 *                | "read input" (<identifier> ",")* <identifier>
 *                | "call service" <stringLiteral>
 *                | "switch" ":" INDENT (<ifThen>)+ [otherwiseThen] DEDENT
 *
 *  <ifThen> ::= <condition> "->" INDENT <block> DEDENT
 *
 *  <otherwiseThen> ::= "otherwise" "->" INDENT <block> DEDENT
 *
 *  <condition> ::= <identifier> "==" <stringLiteral>
 * }}}
 */
class LogicParserSpec extends AnyWordSpec with Matchers {
  val code: String =
    """
      |read input name, country
      |switch:
      |  country == "PT" ->
      |    call service "A"
      |    exit
      |  otherwise ->
      |    call service "B"
      |    switch:
      |      name == "unknown" ->
      |        exit
      |      otherwise ->
      |        call service "C"
      |        exit""".stripMargin

  "Lexer" should {
    "parse tokens" in {
      // pprint.pprintln(WorkflowLexer(code))
    }
  }

  "Compiler" should {
    "parse lexer with parser" in {
      // pprint.pprintln(WorkflowCompiler(code))
    }
  }
}

sealed trait WorkflowToken

final case class IDENTIFIER(str: String) extends WorkflowToken

final case class LITERAL(str: String) extends WorkflowToken

final case class INDENTATION(spaces: Int) extends WorkflowToken

case object EXIT extends WorkflowToken

case object READINPUT extends WorkflowToken

case object CALLSERVICE extends WorkflowToken

case object SWITCH extends WorkflowToken

case object OTHERWISE extends WorkflowToken

case object COLON extends WorkflowToken

case object ARROW extends WorkflowToken

case object EQUALS extends WorkflowToken

case object COMMA extends WorkflowToken

case object INDENT extends WorkflowToken

case object DEDENT extends WorkflowToken

trait WorkflowCompilationError

final case class WorkflowLexerError(msg: String) extends WorkflowCompilationError

object WorkflowLexer extends RegexParsers {
  override def skipWhitespace = true

  /**
   * We cannot ignore \n, since we need it to recognize the level of identation defined by the number of spaces that follow it.
   * Every other whitespace character can be ignored.
   */
  override val whiteSpace: Regex = "[ \t\r\f]+".r

  /**
   * The ^^ operator acts as a map over the parse result.
   * The regex "[a-zA-Z_][a-zA-Z0-9_]*".r is implicitly converted to an instance of Parser[String],
   * on which we map a function (String => IDENTIFIER), thus returning a instance of Parser[IDENTIFIER]
   */
  lazy val identifier: Parser[IDENTIFIER] =
    "[a-zA-Z_][a-zA-Z0-9_]*".r ^^ { str => IDENTIFIER(str) }

  lazy val literal: Parser[LITERAL] =
    """"[^"]*"""".r ^^ { str =>
      val content = str.substring(1, str.length - 1)
      LITERAL(content)
    }

  lazy val indentation: Parser[INDENTATION] =
    "\n[ ]*".r ^^ { whitespace =>
      val nSpaces = whitespace.length - 1
      INDENTATION(nSpaces)
    }

  lazy val exit: Parser[EXIT.type] = "exit" ^^ (_ => EXIT)

  lazy val readInput: Parser[READINPUT.type] = "read input" ^^ (_ => READINPUT)

  lazy val callService: Parser[CALLSERVICE.type] = "call service" ^^ (_ => CALLSERVICE)

  lazy val switch: Parser[SWITCH.type] = "switch" ^^ (_ => SWITCH)

  lazy val otherwise: Parser[OTHERWISE.type] = "otherwise" ^^ (_ => OTHERWISE)

  lazy val colon: Parser[COLON.type] = ":" ^^ (_ => COLON)

  lazy val arrow: Parser[ARROW.type] = "->" ^^ (_ => ARROW)

  lazy val equals: Parser[EQUALS.type] = "==" ^^ (_ => EQUALS)

  lazy val comma: Parser[COMMA.type] = "," ^^ (_ => COMMA)

  /**
   *  - (or), for recognizing any of our token parsers
   *  - rep1, which recognizes one or more repetitions of its argument
   *  - phrase, which attempts to consume all input until no more is left
   */
  lazy val tokens: Parser[List[WorkflowToken]] =
    phrase(rep1(exit | readInput | callService | switch | otherwise | colon | arrow | equals | comma | literal | identifier | indentation)) ^^ { rawTokens =>
      processIndentations(rawTokens)
    }

  /**
   * We apply a brief post-processing step to our parse result with the processIndentations method.
   * This is used to produce the artifical INDENT and DEDENT tokens from the INDENTATION tokens.
   * Each increase in indentation level will be pushed to a stack, producing an INDENT, and decreases in indentation level will be popped from the indentation stack, producing DEDENTs.
   */
  def processIndentations(tokens: List[WorkflowToken], indents: List[Int] = List(0)): List[WorkflowToken] =
    tokens.headOption match {
      // If there is an increase in indentation level, we push this new level into the stack and produce an INDENT
      case Some(INDENTATION(spaces)) if spaces > indents.head =>
        INDENT :: processIndentations(tokens.tail, spaces :: indents)

      // If there is a decrease, we pop from the stack until we have matched the new level, producing a DEDENT for each pop
      case Some(INDENTATION(spaces)) if spaces < indents.head =>
        val (dropped, kept) = indents.partition(_ > spaces)
        (dropped map (_ => DEDENT)) ::: processIndentations(tokens.tail, kept)

      // If the indentation level stays unchanged, no tokens are produced
      case Some(INDENTATION(spaces)) if spaces == indents.head =>
        processIndentations(tokens.tail, indents)

      // Other tokens are ignored
      case Some(token) =>
        token :: processIndentations(tokens.tail, indents)

      // The final step is to produce a DEDENT for each indentation level still remaining, thus "closing" the remaining open INDENTS
      case None =>
        indents.filter(_ > 0).map(_ => DEDENT)
    }

  def apply(code: String): WorkflowLexerError Either List[WorkflowToken] =
    parse(tokens, code) match {
      case Success(result, next) => Right(result)
      case NoSuccess(msg, next)  => Left(WorkflowLexerError(msg))
    }
}

/**
 * Now that we have taken care of the lexical analysis, we are still missing the syntactic analysis step,
 * i.e. transforming a sequence of tokens into an abstract syntax tree (AST).
 * Unlike RegexParsers which generate String parsers, we will be needing a WorkflowToken parser.
 */
object WorkflowParser extends Parsers {
  override type Elem = WorkflowToken

  lazy val identifier: Parser[IDENTIFIER] =
    accept("identifier", { case id @ IDENTIFIER(name) => id })

  lazy val literal: Parser[LITERAL] =
    accept("string literal", { case lit @ LITERAL(name) => lit })

  lazy val condition: Parser[Equals] =
    (identifier ~ EQUALS ~ literal) ^^ { case id ~ eq ~ lit => Equals(id.str, lit.str) }

  lazy val ifThen: Parser[IfThen] =
    (condition ~ ARROW ~ INDENT ~ block ~ DEDENT) ^^ {
      case cond ~ _ ~ _ ~ block ~ _ => IfThen(cond, block)
    }

  lazy val otherwiseThen: Parser[OtherwiseThen] =
    (OTHERWISE ~ ARROW ~ INDENT ~ block ~ DEDENT) ^^ {
      case _ ~ _ ~ _ ~ block ~ _ => OtherwiseThen(block)
    }

  lazy val statement: Parser[WorkflowAST] = {
    val exit: Parser[Exit.type] = EXIT ^^ (_ => Exit)

    val readInput: Parser[ReadInput] =
      READINPUT ~ rep(identifier ~ COMMA) ~ identifier ^^ {
        case read ~ inputs ~ IDENTIFIER(lastInput) => ReadInput(inputs.map(_._1.str) ++ List(lastInput))
      }

    val callService: Parser[CallService] =
      CALLSERVICE ~ literal ^^ {
        case call ~ LITERAL(serviceName) => CallService(serviceName)
      }

    val switch: Parser[Choice] =
      SWITCH ~ COLON ~ INDENT ~ rep1(ifThen) ~ opt(otherwiseThen) ~ DEDENT ^^ {
        case _ ~ _ ~ _ ~ ifs ~ otherwise ~ _ => Choice(ifs ++ otherwise)
      }

    exit | readInput | callService | switch
  }

  lazy val block: Parser[WorkflowAST] =
    rep1(statement) ^^ { stmtList => stmtList reduceRight AndThen }

  lazy val program: Parser[WorkflowAST] =
    phrase(block)

  def apply(tokens: Seq[WorkflowToken]): WorkflowParserError Either WorkflowAST = {
    val reader = new WorkflowTokenReader(tokens)

    program(reader) match {
      case Success(result, next) => Right(result)
      case NoSuccess(msg, next)  => Left(WorkflowParserError(msg))
    }
  }
}

final case class WorkflowParserError(msg: String) extends WorkflowCompilationError

/**
 * We also need to define a Reader[WorkflowToken] which will be used by the parser to read from a sequence of WorkflowToken.
 */
class WorkflowTokenReader(tokens: Seq[WorkflowToken]) extends Reader[WorkflowToken] {
  override def first: WorkflowToken = tokens.head

  override def atEnd: Boolean = tokens.isEmpty

  override def pos: Position = NoPosition

  override def rest: Reader[WorkflowToken] = new WorkflowTokenReader(tokens.tail)
}

sealed trait WorkflowAST

final case class AndThen(step1: WorkflowAST, step2: WorkflowAST) extends WorkflowAST

final case class ReadInput(inputs: Seq[String]) extends WorkflowAST

final case class CallService(serviceName: String) extends WorkflowAST

final case class Choice(alternatives: Seq[ConditionThen]) extends WorkflowAST

case object Exit extends WorkflowAST

sealed trait ConditionThen {
  def thenBlock: WorkflowAST
}

final case class IfThen(predicate: Condition, thenBlock: WorkflowAST) extends ConditionThen

final case class OtherwiseThen(thenBlock: WorkflowAST) extends ConditionThen

sealed trait Condition

final case class Equals(factName: String, factValue: String) extends Condition

object WorkflowCompiler {
  def apply(code: String): WorkflowCompilationError Either WorkflowAST =
    for {
      tokens <- WorkflowLexer(code)
      ast    <- WorkflowParser(tokens)
    } yield ast
}