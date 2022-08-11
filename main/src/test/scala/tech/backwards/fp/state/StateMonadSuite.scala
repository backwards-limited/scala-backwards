package tech.backwards.fp.state

import scala.io.Source
import scala.util.chaining.scalaUtilChainingOps
import scala.util._
import cats.Monad
import cats.data.StateT
import cats.effect.{IO, IOApp}
import cats.implicits._
import monocle.macros.GenLens
import monocle.{Lens, PLens}
import munit.FunSuite

class StateMonadSuite extends FunSuite {
  test("Game State introduction") {
    final case class GameState(
      iHaveTheBall: Boolean,
      fieldPosition: Int,
      down: Int,
      distance: Int,
      myScore: Int,
      computerScore: Int,
      timeRemaining: Int
    )

    val state1: GameState =
      GameState(
        iHaveTheBall = true,
        fieldPosition = 25,
        down = 1,
        distance = 10,
        myScore = 0,
        computerScore = 0,
        timeRemaining = 3600
      )

    val state2: GameState =
      GameState(
        iHaveTheBall = true,
        fieldPosition = 29,
        down = 2,
        distance = 6,
        myScore = 0,
        computerScore = 0,
        timeRemaining = 3536
      )

    // When it's the computer's go (as my playing opponent) it will use the State to determine a list of OffensivePlay e.g.
    final case class OffensivePlay()

    def determinePossiblePlays(s: GameState): List[OffensivePlay] = ???

    val possiblePlays: List[OffensivePlay] =
      determinePossiblePlays(state2)

    /*
    The (American) football game has about 150 GameState instances for every game.
    In the context of functional programming, this raises an interesting question:
    In Scala/FP I can only have val instances, so how can I possibly create 150 new variables for each game?
    Put another way, if you assume that I keep all of the plays in a List, the question becomes,
    "How do I append GameState values to an immutable List?"
    */
  }

  test("Game State for Coin Flip - using recursion and immutable state") {
    // For this one, we'll code an app - See below, CoinFlipApp
  }
}

object CoinFlipApp1 extends IOApp.Simple {
  final case class GameState(numFlips: Int, numCorrect: Int)

  val numFlipsL: Lens[GameState, Int] =
    GenLens[GameState](_.numFlips)

  val numCorrectL: Lens[GameState, Int] =
    GenLens[GameState](_.numCorrect)

  val showPrompt: IO[Unit] =
    IO.print("\n(h)eads, (t)ails, or (q)uit: ")

  val userInput: IO[String] =
    IO.readLine.map(_.trim.toUpperCase)

  val flipRequest: String => Boolean =
    r => r == "H" || r == "T"

  val printableFlip: String => String = {
    case "H" => "Heads"
    case "T" => "Tails"
  }

  val printResultAndGameState: (String, GameState) => IO[Unit] =
    (printableResult, gameState) =>
      IO.print(s"Flip was $printableResult. ") >> printGameState(gameState)

  val printGameState: GameState => IO[Unit] =
    gameState => IO.println(s"#Flips: ${gameState.numFlips}, #Correct: ${gameState.numCorrect}")

  val printGameOver: IO[Unit] =
    IO.println("\n=== GAME OVER ===")

  val flipCoin: Random => String =
    _.nextInt(2) match {
      case 0 => "H"
      case 1 => "T"
    }

  val scored: (String, String) => Int =
    (x, y) => if (x equalsIgnoreCase y) 1 else 0

  def run: IO[Unit] =
    play(GameState(0, 0), new Random).flatMap(gameState =>
      printGameOver >> printGameState(gameState)
    )

  /**
   * a) prompt the user for input
   * b) get the user's input
   * c) flip the coin
   * d) compare the flip result to the user's input
   * e) write the output
   * f) if the user didn't type 'q', play (loop) again
   */
  def play(gameState: GameState, random: Random): IO[GameState] =
    showPrompt >>
    userInput.flatMap(input =>
      IO(flipRequest(input)).ifM(
        IO(printableFlip(input))
          .as(flipCoin(random))
          .flatMap(flip =>
            (numFlipsL.modify(_ + 1) andThen numCorrectL.modify(_ + scored(input, flip)))(gameState).pipe(nextGameState =>
              printResultAndGameState(printableFlip(flip), nextGameState) >> play(nextGameState, random)
            )
          ),
        IO(gameState)
      )
    )
}

/**
 * Add ability to start a new game
 */
object CoinFlipApp2 extends IOApp.Simple {
  final case class GameState(numFlips: Int, numCorrect: Int)

  val numFlipsL: Lens[GameState, Int] =
    GenLens[GameState](_.numFlips)

  val numCorrectL: Lens[GameState, Int] =
    GenLens[GameState](_.numCorrect)

  val showPrompt: IO[Unit] =
    IO.print("\n(h)eads, (t)ails, (n)ew game or (q)uit: ")

  val userInput: IO[String] =
    IO.readLine.map(_.trim.toUpperCase)

  val flipRequest: String => Boolean =
    r => r == "H" || r == "T"

  val printableFlip: String => String = {
    case "H" => "Heads"
    case "T" => "Tails"
  }

  val printResultAndGameState: (String, GameState) => IO[Unit] =
    (printableResult, gameState) =>
      IO.print(s"Flip was $printableResult. ") >> printGameState(gameState)

  val printGameState: GameState => IO[Unit] =
    gameState => IO.println(s"#Flips: ${gameState.numFlips}, #Correct: ${gameState.numCorrect}")

  val printGameOver: IO[Unit] =
    IO.println("\n=== GAME OVER ===")

  val flipCoin: Random => String =
    _.nextInt(2) match {
      case 0 => "H"
      case 1 => "T"
    }

  val scored: (String, String) => Int =
    (x, y) => if (x equalsIgnoreCase y) 1 else 0

  val newGame: GameState =
    GameState(0, 0)
  def run: IO[Unit] =
    play(newGame, new Random).flatMap(gameState =>
      printGameOver >> printGameState(gameState)
    )

  /**
   * a) prompt the user for input
   * b) get the user's input
   * c) flip the coin
   * d) compare the flip result to the user's input
   * e) write the output
   * f) if the user didn't type 'q', play (loop) again
   */
  def play(gameState: GameState, random: Random): IO[GameState] =
    showPrompt >>
    userInput >>= {
      case "N" =>
        printGameOver >> printGameState(gameState) >> play(newGame, random)
      case input @ ("H" | "T") =>
        IO(printableFlip(input)).as(flipCoin(random)) >>= (flip =>
          (numFlipsL.modify(_ + 1) andThen numCorrectL.modify(_ + scored(input, flip)))(gameState).pipe(nextGameState =>
            printResultAndGameState(printableFlip(flip), nextGameState) >> play(nextGameState, random)
          )
        )
      case _ =>
        IO(gameState)
    }
}

/**
 * Keep history of all games
 */
object CoinFlipApp3 extends IOApp.Simple {
  final case class Game(current: GameState, history: List[GameState])
  final case class GameState(numFlips: Int, numCorrect: Int)

  val currentGameL: Lens[Game, GameState] =
    GenLens[Game](_.current)

  val gameHistoryL: Lens[Game, List[GameState]] =
    GenLens[Game](_.history)

  val numFlipsL: Lens[GameState, Int] =
    GenLens[GameState](_.numFlips)

  val currentGameNumFlipsL: PLens[Game, Game, Int, Int] =
    currentGameL andThen numFlipsL

  val numCorrectL: Lens[GameState, Int] =
    GenLens[GameState](_.numCorrect)

  val currentGameNumCorrectL: PLens[Game, Game, Int, Int] =
    currentGameL andThen numCorrectL

  val showPrompt: IO[Unit] =
    IO.print("\n(h)eads, (t)ails, (n)ew game or (q)uit: ")

  val userInput: IO[String] =
    IO.readLine.map(_.trim.toUpperCase)

  val flipRequest: String => Boolean =
    r => r == "H" || r == "T"

  val printableFlip: String => String = {
    case "H" => "Heads"
    case "T" => "Tails"
  }

  val printResultAndGameState: (String, GameState) => IO[Unit] =
    (printableResult, gameState) =>
      IO.print(s"Flip was $printableResult. ") >> printGameState(gameState)

  val printGameState: GameState => IO[Unit] =
    gameState => IO.println(s"#Flips: ${gameState.numFlips}, #Correct: ${gameState.numCorrect}")

  val printGameHistory: List[GameState] => IO[Unit] =
    gameHistory => IO.whenA(gameHistory.nonEmpty)(IO.println("Previous games:") >> gameHistory.traverse_(printGameState))

  val printGameOver: IO[Unit] =
    IO.println("\n=== GAME OVER ===")

  val flipCoin: Random => String =
    _.nextInt(2) match {
      case 0 => "H"
      case 1 => "T"
    }

  val scored: (String, String) => Int =
    (x, y) => if (x equalsIgnoreCase y) 1 else 0

  val newGameState: GameState =
    GameState(0, 0)

  def run: IO[Unit] =
    play(Game(newGameState, Nil), new Random).flatMap(game =>
      printGameOver >> printGameState(game.current) >> printGameHistory(game.history)
    )

  /**
   * a) prompt the user for input
   * b) get the user's input
   * c) flip the coin
   * d) compare the flip result to the user's input
   * e) write the output
   * f) if the user didn't type 'q', play (loop) again
   */
  def play(game: Game, random: Random): IO[Game] =
    showPrompt >>
    userInput >>= {
      case "N" =>
        printGameOver >> printGameState(game.current) >> play((currentGameL.replace(newGameState) andThen gameHistoryL.modify(_ :+ game.current))(game), random)
      case input @ ("H" | "T") =>
        IO(printableFlip(input)).as(flipCoin(random)) >>= (flip =>
          (currentGameNumFlipsL.modify(_ + 1) andThen currentGameNumCorrectL.modify(_ + scored(input, flip))) (game).pipe(game =>
            printResultAndGameState(printableFlip(flip), game.current) >> play(game, random)
          )
        )
      case _ =>
        IO(game)
    }
}

object ResourceWithIOApp extends IOApp.Simple {
  def read(filename: String): Try[List[String]] =
    Using(Source.fromFile(filename))(_.getLines.toList)

  def readIO(filename: String): IO[Unit] =
    IO(read(filename)) >>= {
      case Success(contents) =>
        IO.println(contents)
      case Failure(t) =>
        IO.println(s"Read failure: ${t.getMessage}")
    }

  def run: IO[Unit] =
    readIO(".sbtopts") >> readIO("blah.txt")
}

/**
 * We manually carry over the state
 */
object CustomStateProgressingToStateMonadVersion1 extends IOApp.Simple {
  final case class State(value: Int) {
    def flatMap(f: Int => State): State =
      f(value)

    def map(f: Int => Int): State =
      State(f(value))
  }

  def run: IO[Unit] =
    IO(
      State(20).flatMap(a =>
        State(a + 15).flatMap(b =>
          State(b + 0)
        )
      )
    ).flatMap(s => IO.println(s)) >>
    IO(
      for {
        a <- State(20)
        b <- State(a + 15) // Here we manually carry over the state, in this case "a"
        c <- State(b + 0)  // Here we manually carry over the state, in this case "b"
      } yield c
    ).flatMap(s => IO.println(s))
}

object CustomStateProgressingToStateMonadVersion2 extends IOApp.Simple {
  final case class State[S, A](run: S => (S, A)) {
    def flatMap[B](g: A => State[S, B]): State[S, B] =
      State { (s0: S) =>
        val (s1, a) = run(s0)
        g(a).run(s1)
      }

    def map[B](f: A => B): State[S, B] =
      flatMap(a => State.point(f(a)))
  }

  object State {
    def point[S, A](a: A): State[S, A] = State(s => (s, a))
  }

  final case class S(i: Int)

  def action(i: Int): State[S, Int] =
    State { (s: S) =>
      (S(s.i + i), s.i + i)
    }

  def run: IO[Unit] = {
    val r: (S, Int) =
      action(20).run(S(0))

    println(r)

    action(20).flatMap(_ => action(15)).run(S(0))
    /*
    action(20) =

    res0 =
      State { (s: S) =>
        (S(s.i + 20), s.i + 20)
      }

    Call flatMap on the above which gives a new State =

    res1 =
      g: A => action(15)
      and
      State { (s0: S) =>
        val (s1, a) = run(s0) // This run belongs to res0
        g(a).run(s1)
      }

    run(s0) to give:

    State { (s0: S) =>
      val (s1, a) = (S(s0.i + 20), s0.i + 20)
      g(a).run(s1)
    }

    Finally run(S(0))

    State { (s0 = S(0) =>
      val (s1, a) = (S(s0.i + 20), s0.i + 20)
      g(a).run(s1)
    }

    becomes

    State { (s0 = S(0) =>
      val (S(20), 20) = (S(0 + 20), 0 + 20)
      g(20).run(S(20))
    }

    g(20) =

    20 => action(15).run(S(20))
    =
    State { (s: S(20)) =>
      (S(20 + 15), 20 + 15)
    }
    =
    (S(35), 35)
    */

    IO(
      action(20).flatMap(_ =>
        action(15).flatMap(_ =>
          action(0)
        )
      )
    ).map(_.run(S(0))).flatMap(sAndi => IO.println(sAndi)) >>
    IO(
      for {
        _ <- action(20)
        _ <- action(15)
        i <- action(0)
      } yield i
    ).map(_.run(S(0))).flatMap(sAndi => IO.println(sAndi))
  }
}

object ExampleImplementationOfStateTApp extends IOApp.Simple {
  case class StateT[M[_]: Monad, S, A](run: S => M[(S, A)]) {
    def flatMap[B](g: A => StateT[M, S, B]): StateT[M, S, B] =
      StateT { (s0: S) =>
        Monad[M].flatMap(run(s0)) {
          case (s1, a) => g(a).run(s1)
        }
    }

    def map[B](f: A => B): StateT[M, S, B] =
      flatMap(a => StateT.point(f(a)))
  }

  object StateT {
    def point[M[_]: Monad, S, A](a: A): StateT[M, S, A] =
      StateT(s => Monad[M].pure((s, a)))
  }

  def run: IO[Unit] = ???
}

object CoinFlipAppUsingStateTApp extends IOApp.Simple {
  final case class GameState(numFlips: Int, numCorrect: Int)

  val numFlipsL: Lens[GameState, Int] =
    GenLens[GameState](_.numFlips)

  val numCorrectL: Lens[GameState, Int] =
    GenLens[GameState](_.numCorrect)

  val showPrompt: IO[Unit] =
    IO.print("\n(h)eads, (t)ails, (n)ew game or (q)uit: ")

  val userInput: IO[String] =
    IO.readLine.map(_.trim.toUpperCase)

  val flipRequest: String => Boolean =
    r => r == "H" || r == "T"

  val printableFlip: String => String = {
    case "H" => "Heads"
    case "T" => "Tails"
  }

  val printFlip: String => IO[Unit] =
    flip => IO.println(s"Coin flipped = $flip")

  val printGameState: StateT[IO, GameState, Unit] =
    StateT.inspectF(gameState => IO.println(s"#Flips: ${gameState.numFlips}, #Correct: ${gameState.numCorrect}"))

  val printGameOver: IO[Unit] =
    IO.println("\n=== GAME OVER ===")

  val flipCoin: Random => String =
    _.nextInt(2) match {
      case 0 => "H"
      case 1 => "T"
    }

  val scored: (String, String) => Int =
    (x, y) => if (x equalsIgnoreCase y) 1 else 0

  val score: String => String => StateT[IO, GameState, Unit] =
    input => flip => StateT.modify(numFlipsL.modify(_ + 1) andThen numCorrectL.modify(_ + scored(input, flip)))

  val newGame: GameState =
    GameState(0, 0)

  def run: IO[Unit] =
    (play(new Random) >> StateT.liftF(printGameOver) >> printGameState) runA newGame

  /**
   * a) prompt the user for input
   * b) get the user's input
   * c) flip the coin
   * d) compare the flip result to the user's input
   * e) write the output
   * f) if the user didn't type 'q', play (loop) again
   */
  def play(random: Random): StateT[IO, GameState, Unit] =
    StateT.liftF[IO, GameState, Unit](showPrompt) >>
    StateT.liftF(userInput) >>= {
      case "N" =>
        StateT.liftF[IO, GameState, Unit](printGameOver) >> printGameState >> StateT.set(newGame) >> play(random)
      case input @ ("H" | "T") =>
        StateT.liftF[IO, GameState, String](IO(flipCoin(random)).flatTap(printableFlip andThen printFlip)).flatMap(score(input)) >> printGameState >> play(random)

      case _ =>
        StateT.liftF(IO.unit)
    }
}