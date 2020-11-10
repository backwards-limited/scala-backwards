package com.backwards.fp.free

import scala.annotation.tailrec
import scala.collection.mutable
import org.scalatest.matchers.must.Matchers
import org.scalatest.wordspec.AnyWordSpec

/**
 * [[https://levelup.gitconnected.com/explain-free-monad-like-i-am-five-part-1-5bee794074bd]]
 *
 * [[https://edward-huang.com/functional-programming/scala/programming/monad/2020/09/20/explain-free-monad-like-i-am-five-part-2/]]
 *
 * A free monad is a construction that allows you to build a monad from any Functor.
 *
 * Another definition says it is an idea to switch from effect functions (that might be impure) to plain data structures representing our domain logic.
 * It is like a wrapper that wraps any ADT into a monadic structure DSL, that you can create a program out of.
 * It separates your program DSL with its interpreter so that you can pick and choose various interpreters.
 */
class FreeMonad2Spec extends AnyWordSpec with Matchers {
  "First attempt" should {
    "be imperative" in {
      object Dsl {
        final case class Todo(id: Long, description: String, isFinished: Boolean)

        sealed trait Action[T] extends Product with Serializable

        final case class Create(description: String) extends Action[Unit]
        final case class Find(id: Long) extends Action[Unit]
        final case class Mark(id: Long) extends Action[Unit]
        final case class Delete(id: Long) extends Action[Unit]
        final case object Read extends Action[Unit]
      }

      import Dsl._

      /*
      We will want to do a series of operations such as Create a "to do".
      Then, we can Find the id belongs to the "to do" List.
      Read the "to do" List, to show all the results of the "to do" list that we had, and Mark the "to do" that we have finished.
      */

      // The imperative way of doing such things can be to construct a List of these ADT like this:
      val program = List(
        Create("Do Laundry"),
        Mark(0L),
        Read
      )

      /*
      Therefore, the program above is just a description.
      To execute the plan, we need some executing interpreter to run it.
      Let’s create an “execute” interpreter:
      */
      object Interpreter {
        var map: mutable.Map[Long, Todo] =
          scala.collection.mutable.Map[Long, Todo]()

        var id = 0L

        def execute[T](action: Action[T]): T =
          action match {
            case Read =>
              println(s"Read: ${map.values.toList}")
              ().asInstanceOf[T]

            case Find(id) =>
              println(s"Find: ${map.get(id)}")
              map.get(id).asInstanceOf[T]

            case Mark(id) =>
              println(s"Marking $id")

              println(
                map
                  .get(id)
                  .flatMap(t => map.put(id, t.copy(isFinished = !t.isFinished)))
              )

              map
                .get(id)
                .flatMap(t => map.put(id, t.copy(isFinished = !t.isFinished)))
                .asInstanceOf[T]

            case Delete(id) =>
              println(s"Removing ${map.get(id)}")
              map.remove(id).asInstanceOf[T]

            case Create(description) =>
              println(s"Creating todo list $description in id $id")
              map += (id -> Todo(id, description, isFinished = false))
              id += 1
              ().asInstanceOf[T]
          }
      }

      program foreach Interpreter.execute

      /*
      Now, ideally, we want to do a sequential operation, such as creating a "to do" List, Reading all the list of todos, and marking the finished ones.

      We need to find a way to get the previous operation’s value and do some other sequential process based on the prior operation’s evaluated value.
      Sounds like a Monad, right?
      */

      /*
      Ideally, we want to do something like this:

      val program = for {
        to_do <- Create("Do Laundry")
        listTodos <- Read
        idZero <- Find(0L)
        _ <- Mark(to_do.id)
      } yield ()
      */

      /*
      The problem now is that since the program is not a List anymore, how do we create the interpreter?
      Since we are creating a general data structure on the program,
      we want to have some sort of “wrapper” to wrap these data structures with a Monadic bind to construct a monadic type of program.
      */
    }
  }

  "Second attempt" should {
    "be a rewrite" in {
      final case class Todo(id: Long, description: String, isFinished: Boolean)

      sealed trait Action[T] extends Product with Serializable

      final case class Create(description: String) extends Action[Todo]
      final case class Find(id: Long) extends Action[Option[Todo]]
      final case class Mark(id: Long) extends Action[Unit]
      final case class Delete(id: Long) extends Action[Option[Todo]]
      final case object Read extends Action[List[Todo]]

      // We introduce FlatMap and Pure to bind our original algebraic type to Monad:
      sealed trait Free[F[_], A]

      final case class FlatMap[F[_], A, B](fa: F[A], f: A => F[B]) extends Free[F, B]

      final case class Pure[F[_], A](fa: F[A]) extends Free[F, A]

      /*
      Writing a wrapper of our program so that it can have that monadic bind function.
      The monadic operation can be translated to something like this:
      FlatMap(Create("Do Laundry"), to_do =>
        FLatMap(Create("Clean Bedroom"), cleanBedroom =>
          FlatMap(Read, (listTodos) =>
            FlatMap(Find(0L), idZero =>
              Pure(Mark(idZero))
            )
          )
        )
      )

      And to write as a for-comprehension e.g.

      val program =
        for {
          to_do       <- Create("Do Laundry")
          _           <- Mark(to_do.id)
          listOfTodo  <- Read
        } yield
          println(listOfTodo)

      1. Free has to be a monad.
         It needs to have some flatMap and map so that scala can detect and do "for-comprehension".
      2. We want the program to do flatMap on Free, not the "Action" ADT that we defined.
         That leaves the action just a data structure that we can wire to our interpreter later on.
      */
    }
  }

  "Third attempt" should {
    "be our Free Monad" in {
      final case class Todo(id: Long, description: String, isFinished: Boolean)

      sealed trait Action[T] extends Product with Serializable

      final case class Create(description: String) extends Action[Todo]
      final case class Find(id: Long) extends Action[Option[Todo]]
      final case class Mark(id: Long) extends Action[Unit]
      final case class Delete(id: Long) extends Action[Option[Todo]]
      final case object Read extends Action[List[Todo]]


      sealed trait Free[F[_], A] {
        import Free._

        def flatMap[B](fn: A => Free[F, B]): Free[F, B] = this match {
          case Free.Pure(a) =>
            fn(a)

          case Free.FlatMap(fa, fun) =>
            FlatMap(fa, fun andThen (a => a.flatMap(fn)))
        }

        def map[B](fn: A => B): Free[F, B] =
          flatMap(a => Pure(fn(a)))
      }

      object Free {
        final case class FlatMap[F[_], A, B](fa: F[A], fn: A => Free[F, B]) extends Free[F, B]

        final case class Pure[F[_], A](a: A) extends Free[F, A]
      }

      // We want to lift the Action to a Free[F[_], A].
      // Let’s create a lift a function that will do that:
      import Free._

      implicit def lift[A](fa: Action[A]): Free[Action, A] = FlatMap(fa, Pure[Action, A])

      val program =
        for {
          todo        <- Create("Do Laundry")
          _           <- Mark(todo.id)
          listOfTodo  <- Read
        } yield
          println(listOfTodo)

      // Now that we have created our program, we also need to bind the program with an interpreter somehow.

      var map = scala.collection.mutable.Map[Long, Todo]()
      var id = 0L

      def execute[T](action: Action[T]): T =
        action match {
          case Read =>
            println(map.values.toList)
            map.values.toList.asInstanceOf[T]

          case Find(id) =>
            println(map.values.find(t => t.id == id))
            map.values.find(t => t.id == id).asInstanceOf[T]

          case Mark(id) =>
            println(
              map
                .get(id)
                .flatMap(t => map.put(id, t.copy(isFinished = !t.isFinished)))
            )

            ().asInstanceOf[T]

          case Delete(id) =>
            println(s"removing ${map.get(id)}")
            map.remove(id).asInstanceOf[T]

          case Create(description) =>
            println(s"creating todo list $description in id $id")
            val todo = Todo(id, description, isFinished = false)
            map += (id -> Todo(id, description, isFinished = false))
            id += 1
            todo.asInstanceOf[T]
        }

      def runProgram[A](program: Free[Action, A]): A = program match {
        case Free.Pure(a) =>
          a

        case FlatMap(fa: Action[A], fn: (A => Free[Action, A])) =>
          // execute the Action here
          val res = execute(fa)
          // thread the function into a new Free
          val newFree = fn(res)
          // execute the next function
          runProgram(newFree)
      }

      runProgram(program)
    }
  }
}