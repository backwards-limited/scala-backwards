package com.backwards.fp.free

import scala.collection.mutable
import cats.data.State
import cats.free.Free
import cats.free.Free.liftF
import cats.{Id, ~>}
import org.scalatest.matchers.must.Matchers
import org.scalatest.wordspec.AnyWordSpec

/**
 * A free monad is a construction which allows you to build a monad from any Functor. Like other monads, it is a pure way to represent and manipulate computations.
 *
 * In particular, free monads provide a practical way to:
 * - represent stateful computations as data, and run them
 * - run recursive computations in a stack-safe way
 * - build an embedded DSL (domain-specific language)
 * - retarget a computation to another interpreter using natural transformations
 */
class FreeMonadSpec extends AnyWordSpec with Matchers {
  "Free" should {
    "be demoed from Cats website" in {
      // 1. Create a Free type based on your ADT
      type KVStore[A] = Free[KVStoreAlg, A]

      // 2. Create smart constructors using liftF
      // These methods will make working with our DSL a lot nicer,
      // and will lift KVStoreAlg[_] values into our KVStore[_] monad (note the missing “A” in the second type).

      // What does the (helper) liftF function do?

      // sealed abstract class Free[S[_], A]
      //                  ^
      //                  |
      // case class Suspend[S[_], A](a: S[A]) extends Free[S, A]

      // liftF[F[_], A](value: F[A]): Free[F, A] = Suspend(value)

      // Put returns nothing (i.e. Unit).
      def put[T](key: String, value: T): KVStore[Unit] =
        liftF[KVStoreAlg, Unit](Put[T](key, value))

      // Get returns a T value.
      def get[T](key: String): KVStore[Option[T]] =
        liftF[KVStoreAlg, Option[T]](Get[T](key))

      // Delete returns nothing (i.e. Unit).
      def delete(key: String): KVStore[Unit] =
        liftF(Delete(key))

      // Update composes get and set, and returns nothing.
      def update[T](key: String, f: T => T): KVStore[Unit] =
        for {
          vMaybe <- get[T](key)
          _ <- vMaybe.map(v => put[T](key, f(v))).getOrElse(Free.pure(()))
        } yield ()

      // 3. Build a program
      // The following looks like a monadic flow. However, it just builds a recursive data structure representing the sequence of operations.

      def program: KVStore[Option[Int]] =
        for {
          _ <- put("wild-cats", 2)
          _ <- update[Int]("wild-cats", (_ + 12))
          _ <- put("tame-cats", 5)
          n <- get[Int]("wild-cats")
          _ <- delete("tame-cats")
        } yield n

      // 4. Write a compiler for your program
      // Free[_] is a programming language inside your programming language!
      // So, like any other programming language, we need to compile our abstract language into an effective language and then run it.
      // To do this, we will use a natural transformation between type containers.
      // Natural transformations go between types like F[_] and G[_]
      // (this particular transformation would be written as FunctionK[F,G] or as done here using the symbolic alternative as F ~> G)

      def impureCompiler: KVStoreAlg ~> Id  =
        new (KVStoreAlg ~> Id) {
          // a very simple (and imprecise) key-value store
          val kvs = mutable.Map.empty[String, Any]

          def apply[A](fa: KVStoreAlg[A]): Id[A] = fa match {
            case Put(key, value) =>
              println(s"put($key, $value)")
              kvs(key) = value
              ()

            case Get(key) =>
              println(s"get($key)")
              kvs.get(key).map(_.asInstanceOf[A])

            case Delete(key) =>
              println(s"delete($key)")
              kvs.remove(key)
              ()
          }
        }

      // 5. Run your program
      val result: Option[Int] = program.foldMap(impureCompiler)
      println(result)
      // put(wild-cats, 2)
      // get(wild-cats)
      // put(wild-cats, 14)
      // put(tame-cats, 5)
      // get(wild-cats)
      // delete(tame-cats)
      // result: Option[Int] = Some(14)

      // An important aspect of foldMap is its stack-safety. It evaluates each step of computation on the stack then unstack and restart.
      // This process is known as trampolining.

      // 6. Use a pure compiler (optional)
      type KVStoreState[A] = State[Map[String, Any], A]

      val pureCompiler: KVStoreAlg ~> KVStoreState = new (KVStoreAlg ~> KVStoreState) {
        def apply[A](fa: KVStoreAlg[A]): KVStoreState[A] = fa match {
          case Put(key, value) =>
            State.modify(_.updated(key, value))

          case Get(key) =>
            State.inspect(_.get(key).map(_.asInstanceOf[A]))

          case Delete(key) =>
            State.modify(_ - key)
        }
      }

      val resultPure: (Map[String, Any], Option[Int]) = program.foldMap(pureCompiler).run(Map.empty).value
      println(resultPure)
      // resultPure: (Map[String,Any], Option[Int]) = (Map(wild-cats -> 14),Some(14))
    }
  }
}

sealed trait KVStoreAlg[A]

case class Put[T](key: String, value: T) extends KVStoreAlg[Unit]

case class Get[T](key: String) extends KVStoreAlg[Option[T]]

case class Delete(key: String) extends KVStoreAlg[Unit]
