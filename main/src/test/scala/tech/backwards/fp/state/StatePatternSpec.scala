package tech.backwards.fp.state

import cats.Eval
import cats.data._
import cats.implicits._
import monocle._
import munit.FunSuite

/**
 * https://edward-huang.com/scala/functional-programming/monad/programming/2020/12/21/must-know-patterns-for-constructing-stateful-programs-without-any-mutation/
 */
class StatePatternSpec extends FunSuite {
  test("zipWithIndex version 1") {
    def zipWithIndex[A](xs: List[A]): List[(A, Int)] = {
      val state: IndexedStateT[Eval, Int, Int, List[(A, Int)]] =
        xs.traverse(a =>
          for {
            index <- State.get[Int]
            _ <- State.set[Int](index + 1)
          } yield (a, index)
        )

      state.runA(0).value
    }

    assertEquals(
      zipWithIndex(List("a", "b")),
      List("a" -> 0, "b" -> 1)
    )
  }

  test("zipWithIndex version 2") {
    def zipWithIndex[A](xs: List[A]): List[(A, Int)] = {
      val state: State[Int, List[(A, Int)]] =
        xs.traverse(a =>
          State.get[Int].flatMap(index => State.set[Int](index + 1).map(_ => a -> index))
        )

      state.runA(0).value
    }

    assertEquals(
      zipWithIndex(List("a", "b")),
      List("a" -> 0, "b" -> 1)
    )
  }

  test("zipWithIndex version 3") {
    def zipWithIndex[A](xs: List[A]): List[(A, Int)] = {
      val state: State[Int, List[(A, Int)]] =
        xs.traverse(a =>
          State[Int, (A, Int)](index => (index + 1, a -> index))
        )

      state.runA(0).value
    }

    assertEquals(
      zipWithIndex(List("a", "b")),
      List("a" -> 0, "b" -> 1)
    )
  }

  /**
   * You need to create a game where a robot is standing at one point.
   * There will be a list of Instructions and a destination.
   * You will need to program the robot to validate if the robot has reached the goal based on the List of Instructions that the user is giving.
   */
  test("robot version 1") {
    sealed trait Instruction

    // NOTE - Would not need "final" if these objects were at the top-level
    final case object Up extends Instruction

    final case object Down extends Instruction

    final case object Left extends Instruction

    final case object Right extends Instruction

    // The State will be the starting point (and each subsequent point), and the result will also be a point (a tuple of x, y):
    type Point = (Int, Int)

    type ResultPoint = (Int, Int)

    type RobotState = State[Point, ResultPoint]

    val modifyX: Point => (Int => Int) => Point =
      point => f => Focus[Point](_._1).modify(f)(point)

    val modifyY: Point => (Int => Int) => Point =
      point => f => Focus[Point](_._2).modify(f)(point)

    def op: Instruction => Point => Point = {
      case Up => modifyY(_)(_ + 1)
      case Down => modifyY(_)(_ - 1)
      case Left => modifyX(_)(_ - 1)
      case Right => modifyX(_)(_ + 1)
    }

    def willReach(instructions: List[Instruction], start: Point)(destination: Point): Boolean = {
      val state: State[Point, List[Point]] =
        instructions.traverse { instruction =>
          for {
            currentCoord <- State.get[Point]            // get the current state
            nextCoord = op(instruction)(currentCoord)   // execute the function
            _ <- State.set[Point](nextCoord)            // set the current state
          } yield nextCoord                             // this will turn into a List[Point] later
        }

      val (finalDestination: Point, history: List[Point]) =
        state.run(start).value // run the program by providing initial start point

      finalDestination == destination // check if it is equal
    }

    assert(willReach(List(Up, Up, Up, Down), (0, 0))(0, 2))
  }

  test("robot version 2") {
    sealed trait Instruction

    // NOTE - Would not need "final" if these objects were at the top-level
    final case object Up extends Instruction

    final case object Down extends Instruction

    final case object Left extends Instruction

    final case object Right extends Instruction

    // The State will be the starting point (and each subsequent point), and the result will also be a point (a tuple of x, y):
    type Point = (Int, Int)

    type ResultPoint = (Int, Int)

    type RobotState = State[Point, ResultPoint]

    val modifyX: Point => (Int => Int) => Point =
      point => f => Focus[Point](_._1).modify(f)(point)

    val modifyY: Point => (Int => Int) => Point =
      point => f => Focus[Point](_._2).modify(f)(point)

    def op: Instruction => Point => Point = {
      case Up => modifyY(_)(_ + 1)
      case Down => modifyY(_)(_ - 1)
      case Left => modifyX(_)(_ - 1)
      case Right => modifyX(_)(_ + 1)
    }

    def willReach(instructions: List[Instruction], start: Point)(destination: Point): Boolean = {
      val state: State[Point, List[Point]] =
        instructions.traverse(instruction =>
          State[Point, Point] { currentCoord =>
            val nextCoord = op(instruction)(currentCoord)
            nextCoord -> nextCoord
          }
        )

      val (finalDestination: Point, history: List[Point]) =
        state.run(start).value // run the program by providing initial start point

      finalDestination == destination // check if it is equal
    }

    assert(willReach(List(Up, Up, Up, Down), (0, 0))(0, 2))
  }

  test("robot version 3") {
    sealed trait Instruction

    // NOTE - Would not need "final" if these objects were at the top-level
    final case object Up extends Instruction

    final case object Down extends Instruction

    final case object Left extends Instruction

    final case object Right extends Instruction

    // The State will be the starting point (and each subsequent point), and the result will also be a point (a tuple of x, y):
    type Point = (Int, Int)

    type ResultPoint = (Int, Int)

    type RobotState = State[Point, ResultPoint]

    val modifyX: Point => (Int => Int) => Point =
      point => f => Focus[Point](_._1).modify(f)(point)

    val modifyY: Point => (Int => Int) => Point =
      point => f => Focus[Point](_._2).modify(f)(point)

    def op: Instruction => Point => Point = {
      case Up => modifyY(_)(_ + 1)
      case Down => modifyY(_)(_ - 1)
      case Left => modifyX(_)(_ - 1)
      case Right => modifyX(_)(_ + 1)
    }

    def willReach(instructions: List[Instruction], start: Point)(destination: Point): Boolean = {
      val state: State[Point, List[Point]] =
        instructions.traverse(instruction =>
          State.get[Point].modify(op(instruction))
        )

      val (finalDestination: Point, history: List[Point]) =
        state.run(start).value // run the program by providing initial start point

      println(s"===> ${history.mkString(", ")}")

      finalDestination == destination // check if it is equal
    }

    assert(willReach(List(Up, Up, Up, Down), (0, 0))(0, 2))
  }

  /**
   * Implement a post-order calculator e.g.
   * {{{
   *  1 2 + 3 *   // see 1, push onto stack
   *  2 + 3 *     // see 2, push onto stack
   *  + 3 *       // see +, pop 1 and 2 off of stack,
   *  // push (1 + 2) = 3 in their place
   *  3 3 *       // see 3, push onto stack
   *  3 *         // see 3, push onto stack
   *  *           // see *, pop 3 and 3 off of stack,
   *  // push (3 * 3) = 9 in their place
   * }}}
   */
  test("post order calculator version 1") {
    def execute(f: (Int, Int) => Int): List[Int] => Int = {
      case a :: b :: Nil => f(a, b)
      case _ => sys.error("fail!")
    }

    def transform(token: String)(xs: List[Int]): List[Int] =
      token match {
        case "+" => execute(_ + _)(xs) :: Nil
        case "-" => execute(_ - _)(xs) :: Nil
        case "/" => execute(_ / _)(xs) :: Nil
        case "*" => execute(_ * _)(xs) :: Nil
        case n => n.toInt :: xs
      }

    def eval(xs: List[String]): Int = {
      val state = for {
        _ <- xs.traverse(token =>
          State.modify[List[Int]](transform(token))
        )
        stack <- State.get[List[Int]]
      } yield stack.head

      state.runA(Nil).value
    }

    assertEquals(
      eval(List("1", "2", "+", "3", "*")),
      9
    )
  }

  test("post order calculator version 2") {
    def execute(f: (Int, Int) => Int): List[Int] => Int = {
      case a :: b :: Nil => f(a, b)
      case _ => sys.error("fail!")
    }

    def transform(token: String)(xs: List[Int]): List[Int] =
      token match {
        case "+" => execute(_ + _)(xs) :: Nil
        case "-" => execute(_ - _)(xs) :: Nil
        case "/" => execute(_ / _)(xs) :: Nil
        case "*" => execute(_ * _)(xs) :: Nil
        case n => n.toInt :: xs
      }

    def eval(xs: List[String]): Int = {
      val state: State[List[Int], List[Int]] =
        xs.traverse(token =>
          State.modify[List[Int]](transform(token))
        ).get

      state.runA(Nil).value.head
    }

    assertEquals(
      eval(List("1", "2", "+", "3", "*")),
      9
    )
  }

  test("post order calculator version 3") {
    def execute(f: (Int, Int) => Int): List[Int] => Int = {
      case a :: b :: Nil => f(a, b)
      case _ => sys.error("fail!")
    }

    def transform(token: String)(xs: List[Int]): List[Int] =
      token match {
        case "+" => execute(_ + _)(xs) :: Nil
        case "-" => execute(_ - _)(xs) :: Nil
        case "/" => execute(_ / _)(xs) :: Nil
        case "*" => execute(_ * _)(xs) :: Nil
        case n => n.toInt :: xs
      }

    def eval(xs: List[String]): Int = {
      val state: State[List[Int], List[Unit]] =
        xs.traverse(token =>
          State.modify[List[Int]](transform(token))
        )

      state.runS(Nil).value.head
    }

    assertEquals(
      eval(List("1", "2", "+", "3", "*")),
      9
    )
  }

  test("typical pattern version 1") {
    def mapAccum[A, B, S](xs: List[A], initialState: S)(f: (S, A) => (S, B)): (S, List[B]) =
      xs.traverse(a =>
        for {
          s <- State.get[S]
          (s2, b) = f(s, a)
          _ <- State.set[S](s2)
        } yield b
      ).run(initialState).value

    val (state: String, result: List[Boolean]) =
      mapAccum(List(1, 2, 3), "start:")((s, a) => s + s" $a" -> (a % 2 == 0))

    assertEquals(
      state,
      "start: 1 2 3"
    )

    assertEquals(
      result,
      List(false, true, false)
    )
  }

  test("typical pattern version 2") {
    def mapAccum[A, B, S](xs: List[A], initialState: S)(f: (S, A) => (S, B)): (S, List[B]) =
      xs.traverse(a =>
        State.get[S].flatMap(s =>
          State[S, B](_ => f(s, a))
        )
      ).run(initialState).value

    val (state: String, result: List[Boolean]) =
      mapAccum(List(1, 2, 3), "start:")((s, a) => s + s" $a" -> (a % 2 == 0))

    assertEquals(
      state,
      "start: 1 2 3"
    )

    assertEquals(
      result,
      List(false, true, false)
    )
  }
}