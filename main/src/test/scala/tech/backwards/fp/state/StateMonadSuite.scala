package tech.backwards.fp.state

import scala.util.Random
import scala.util.chaining.scalaUtilChainingOps
import cats.effect.{IO, IOApp}
import munit.FunSuite
import cats.implicits._
import monocle.Lens
import monocle.macros.GenLens

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

object CoinFlipApp extends IOApp.Simple {
  final case class GameState(numFlips: Int, numCorrect: Int)

  val numFlipsL: Lens[GameState, Int] =
    GenLens[GameState](_.numFlips)

  val numCorrectL: Lens[GameState, Int] =
    GenLens[GameState](_.numCorrect)

  val showPrompt: IO[Unit] =
    IO.print("\n(h)eads, (t)ails, or (q)uit: ")

  val userInput: IO[String] =
    IO.readLine.map(_.trim.toUpperCase)

  val printableFlipResult: String => String = {
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
    play(GameState(0, 0), new Random) >> printGameOver

  /**
   * a) prompt the user for input
   * b) get the user's input
   * c) flip the coin
   * d) compare the flip result to the user's input
   * e) write the output
   * f) if the user didn't type 'q', play (loop) again
   */
  def play(gameState: GameState, random: Random): IO[Unit] =
    showPrompt >>
    userInput.flatMap(input =>
      IO.whenA(input != "Q")(
        IO(printableFlipResult(input))
          .as(flipCoin(random))
          .flatMap(flip =>
            (numFlipsL.modify(_ + 1) andThen numCorrectL.modify(_ + scored(input, flip)))(gameState).pipe(nextGameState =>
              printResultAndGameState(printableFlipResult(flip), nextGameState) >>
              play(nextGameState, random)
            )
          )
      )
    )
}