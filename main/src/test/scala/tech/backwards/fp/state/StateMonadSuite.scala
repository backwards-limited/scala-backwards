package tech.backwards.fp.state

import scala.util.Random
import scala.util.chaining.scalaUtilChainingOps
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