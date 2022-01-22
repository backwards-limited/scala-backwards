package tech.backwards.catz.monad

import cats.Monad
import org.scalatest.matchers.must.Matchers
import org.scalatest.wordspec.AnyWordSpec

/**
  * What's a Monad then? It is a type class that extends FlatMap and Applicative.
  *
  * In very simple terms a Monad has a map and flatMap method and we use them for sequencing operations
  * e.g. fetch a user based on her id, then fetch the orders associated with this user.
  * So, a Monad is a Functor (has a map method) but it also has flatten and flatMap methods (flatMap is simply a combination of map and flatten).
  *
  * We use the flatMap method to sequence effectful operations.
  * In simple terms effectful means the operations return monads i.e. Future[String].flatMap(...): Future[A]
  */
class MonadSpec extends AnyWordSpec with Matchers {
  "State" should {
    "describe" in {
      final case class State[S, A](run: S => (A, S)) {
        def compose[B](f: A => State[S, B]): State[S, B] = {
          val composedRuns = (s: S) => {
            val (a, nextState) = run(s)
            f(a).run(nextState)
          }

          State(composedRuns)
        }
      }

      object State {
        def apply[S, A](a: => A): State[S, A] =
          new State(s => (a, s))

        def get[S]: State[S, S] =
          State(s => (s, s))

        def set[S](s: => S): State[S, Unit] =
          State(_ => ((), s))
      }

      State(4).run("First state") mustBe (4, "First state")

      State.get.run("First state") mustBe ("First state", "First state")

      State.set("Second state").run("First state") mustBe ((), "Second state")

      /*
      We would like to be able to compose this computation with the next one.
      We'd like to do this similarly to how we compose functions, but instead of
      (A => B) compose (B => C), we now have
      State[S, A] compose State[S, B]
      How can we do this?
      */

      /*
      By definition, our second computation accepts the result of the first one as its argument, hence we start with (a: A) =>
      We also stated that, as a result (because of the possible state change and return type of the second state), we'll have a State[S, B]
      which gives us a full signature for the computation to compose with the first one:
      f: A => State[S, B]
      (hence the above compose function)
      */

      val state1 = State[String, Int](1)

      val state2 = State[String, Int](2)

      state1.compose(int => state2).run("Initial state") mustBe (2, "Initial state")
      state1.compose(int => State(int)).run("Initial state") mustBe (1, "Initial state")

      /*
      We have an effect and can create a monad for it.
      Note that the signature of the compose method is the same as that of the monadic flatMap.

      The compose function relates to the concept of Kleisli composition.
      It is often called Kleisli arrow, and in essence is just a wrapper over the A => F[B] function,
      which allows for the composition of functions returning monadic values.
      It is frequently named >>=
      */
      implicit def stateMonad[S]: Monad[State[S, *]] = new Monad[State[S, *]] {
        override def pure[A](a: A): State[S, A] = State(a)

        override def flatMap[A, B](a: State[S, A])(f: A => State[S, B]): State[S, B] = a.compose(f)

        override def tailRecM[A, B](a: A)(f: A => State[S, Either[A, B]]): State[S, B] = ???
      }
    }

    "show example" in {
      /*
      We're going fishing by boat.
      The boat has a position and direction, and can go forward for some time or change direction.
      */
      final case class Boat(direction: Double, position: (Double, Double)) {
        def go(speed: Float, time: Float): Boat = {
          val distance = speed * time
          val (x, y) = position
          val nx = x + distance * Math.cos(direction)
          val ny = y + distance * Math.sin(direction)
          copy(direction, (nx, ny))
        }

        def turn(angle: Double): Boat =
          copy(direction = (this.direction + angle) % (2 * Math.PI))
      }

      // We can work with boat e.g.
      val boat = Boat(0, (0d, 0d))

      val finalBoat: Boat = boat.go(10, 5).turn(0.5).go(20, 20).turn(-0.1).go(1, 1)

      /*
      There is a problem with this approach, though — it does not include fuel consumption.
      Unfortunately, this aspect was not envisioned at the time the boat's navigation was developed, and has been added later as a global state.
      We will now refactor the old style with the state monad.
      If the quantity of fuel is modelled as a number of litres, the most straightforward way to define the state is as follows:
      */
      import cats.data.State

      type FuelState = State[Float, Boat]

      /*
      Create pure functions that will take fuel consumption into account while moving the boat.
      Assuming that we can't change the original definition of Boat, we have to pass the boat as a parameter to these functions:
      */
      lazy val consumption = 1f

      def consume(speed: Float, time: Float): Float =
        consumption * time * speed

      def turn(angle: Double)(boat: Boat): FuelState =
        State.pure(boat.turn(angle))

      def go(speed: Float, time: Float)(boat: Boat): FuelState =
        State[Float, Boat](fuel => {
          val newFuel = fuel - consume(speed, time)
          newFuel -> boat.go(speed, time)
        })

      // We can now create the same chain of actions we had defined initially, but this time by tracking fuel consumption:

      def move(boat: Boat): State[Float, Boat] =
        State.pure[Float, Boat](boat)
          .flatMap(go(10, 5))
          .flatMap(turn(0.5))
          .flatMap(go(20, 20))
          .flatMap(turn(-0.1))
          .flatMap(go(1, 1))

      def moveExplicitly(boat: Boat): State[Float, Boat] = for {
        a <- State.pure[Float, Boat](boat)
        b <- go(10, 5)(a)
        c <- turn(0.5)(b)
        d <- go(20, 20)(c)
        e <- turn(-0.1)(d)
        f <- go(1, 1)(e)
      } yield f

      val (fuel, boatDone) = move(boat).run(1000f).value
      println(fuel)
      println(boatDone)
    }
  }
}