package com.backwards.exercism

import org.scalatest.matchers.must.Matchers
import org.scalatest.wordspec.AnyWordSpec

/**
 * [[http://www.doscienceto.it/blog/posts/2020-09-16-change.html 4 Ways to Make Change in Scala]]
 *
 * Correctly determine the fewest number of coins to be given to a customer such that the sum of the coin’s value would equal the correct amount of change.
 */
class ChangeSpec extends AnyWordSpec with Matchers {
  "Attempt One" should {
    "take a long time" ignore {
      println(AttemptOne.findFewestCoins(999, List(1, 2, 5, 10, 20, 50, 100)))
    }
  }
}

object AttemptOne {
  // Explicitly write out some "useful" results
  val noCoins: Option[List[Int]] =
    Some(List.empty[Int])

  val failure: Option[List[Int]] =
    None

  def findFewestCoins(target: Int, coins: List[Int]): Option[List[Int]] =
    (target, coins) match {
      // The value 0 is made up from 0 coins
      case (0, _) => noCoins

      // If we have no "coins" we can't solve the problem
      case (_, Nil) => failure

      // If the target value is negative, we also can't solve the problem
      case (target, _) if target < 0 => failure

      // Otherwise, we want to investigate 2 possibilities
      case (target, h :: tail) =>
        // Either we do use the first coin so we need to know how to make target - it's value
        lazy val usingFirstCoin: Option[List[Int]] =
          findFewestCoins(target - h, coins).map(h :: _)

        // Or we don't, so we need to know how to make the target without using it
        lazy val notUsingFirstCoin: Option[List[Int]] =
          findFewestCoins(target, tail)

        // Of those two possibilities we're interested in the one that involves the fewest coins
        List(usingFirstCoin, notUsingFirstCoin).flatten.minByOption(_.length)
    }
}

object AttemptTwo {
  import cats.data.State
  import cats.syntax.all._

  // Let's define some type aliases, because otherwise our type definitions are going to be really long.
  // We're storing our cached results in a `Map` where the keys are a tuple of the arguments.
  type Memo = Map[(Int, List[Int]), Option[List[Int]]]

  // And that value `Memo` will appear in our `State` Monad
  type MemoState[A] = State[Memo, A]

  // We need some Initial State to run the Monad.
  // At the start of the calculation, we haven't memoized anything yet.
  // So we can just use an empty Map.
  val nothingMemoizedYet: Memo =
    Map.empty

  // Our two base cases, are `None` or `Some(List.empty) in a `MemoState` Context
  val failure: MemoState[Option[List[Int]]] =
    (None: Option[List[Int]]).pure[MemoState]

  val noCoins: MemoState[Option[List[Int]]] =
    Option(List.empty[Int]).pure[MemoState]

  // The function we're trying to memoize has the type `(Int, List[Int]) => Result`
  // This takes a function with those types, and memoizes it via the State Monad
  // I find that writing this in a "generic" way, with type parameters makes it slightly easier to think about
  def memoize[A, R](f: A => State[Map[A, R], R])(a: A): State[Map[A, R], R] =
    for {
      previous <- State.get[Map[A, R]]
      result <- previous.get(a) match {
        case Some(memoizedResult) => State.pure[Map[A, R], R](memoizedResult)
        case None => f(a)
      }
      _ <- State.modify[Map[A, R]](_.updated(a, result))
    } yield result

  // This is _more or less_ the same solution we used previously
  val findFewestCoinsMemo: ((Int, List[Int])) => MemoState[Option[List[Int]]] =
    memoize[(Int, List[Int]), Option[List[Int]]]({
      case (0, _) => noCoins
      case (_, Nil) => failure
      case (t, _) if t < 0 => failure
      case (target, h :: tail) => for {
        usingFirstCoin <- findFewestCoinsMemo(target - h, h :: tail)
        notUsingFirstCoin <- findFewestCoinsMemo(target, tail)
      } yield List(
        usingFirstCoin.map(h :: _),
        notUsingFirstCoin
      ).flatten.minByOption(_.length)
    })(_)

  // Finally "run" our memoized function, using an initial empty state
  def findFewestCoins(target: Int, candidates: List[Int]): Option[List[Int]] =
    findFewestCoinsMemo(target, candidates).run(nothingMemoizedYet).value._2
}