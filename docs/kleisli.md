# Kleisli

We'll follow the blog post [Kleisli Category](https://blog.softwaremill.com/kleisli-category-from-theory-to-cats-fbd140bf396e).

In [category theory](https://en.wikipedia.org/wiki/Category_theory), a **Kleisli category** is a [category](https://en.wikipedia.org/wiki/Category_(mathematics)) naturally associated to any [monad](https://en.wikipedia.org/wiki/Monad_(category_theory)) *T*.

To reach our Kleisli end goal, the blog takes baby steps:

```scala
var logAcc: String = ""

val simple_1: Boolean => Boolean = { x =>
  logAcc = logAcc + s"simple_1 called with $x\n"
  x
}

val simple_2: Boolean => Boolean = { x =>
  logAcc = logAcc + s"simple_2 called with $x\n"
  x
}

simple_1(true)
simple_2(false)
scribe info s"LOG:\n$logAcc"
```

Resulting in:

```bash
simple_1 called with true
simple_2 called with false
```

Now pass logs from one function execution to the next as a parameter:

```scala
/**
 * this version uses pure functions but the functions are:
 * - difficult to memoize
 * - difficult to work with at the end
 * - uses logic for handling logs inside - breaks single repsonsiblitiy principle
 */

val simple_1: (Boolean, String) => (Boolean, String) = { (x: Boolean, log: String) =>
  (!x, log + s"simple_1 called with $x\n")
}

val simple_2: (Boolean, String) => (Boolean, String) = { (x: Boolean, log: String) =>
  (!x, log + s"simple_2 called with $x\n")
}

val negate_1_Result = simple_1(true, "")
val negate_2_Result = simple_2(false, negate_1_Result._2)
scribe info s"LOG:\n${negate_2_Result._2}"
```

Resulting in:

```bash
simple_1 called with true
simple_2 called with false
```

Our functions must remember to concatenate the logs, breaking "single responsibility", so we could do the following to keep this outside, but we are essentially just moving the problem:

```scala
/**
 * this version uses pure functions but the functions are:
 * - difficult to work with at the end - we need to modify the process of composing functions
 */

val simple_1: Boolean => (Boolean, String) = { (x: Boolean) =>
  (!x, s"simple_1 called with $x\n")
}

val simple_2: Boolean => (Boolean, String) = { (x: Boolean) =>
  (!x, s"simple_2 called with $x\n")
}

val negate_1_Result = simple_1(true)
val negate_2_Result = simple_2(false)
scribe info s"LOG:\n${negate_1_Result._2 + negate_2_Result._2}"
```

Resulting in:

```bash
simple_1 called with true
simple_2 called with false
```

With function composition, we take two composable functions and create a single function out of it.

We can compose function `A` -> `B` with function `B` -> `C` which results in function `A` -> `C`.

Composition is fundamental to functional programming. Having three types `A`, `B` and `C` we can create our `compose` function like so:

```scala
def compose[A, B, C](f: A => B, g: B => C): A => C =
	(a: A) => {
  	val b = f(a)
  	val c = g(b)
  	c
	}
```

We can create our own composing function.

The function will take our last two functions taking `Boolean` and returning (`Boolean` ,`String` ) and convert those to a single function which takes a `Boolean` and returns (`Boolean` ,`String` ). For this example we generalize `Boolean` to a generic type `A`:

```scala
val simple_1: Boolean => (Boolean, String) = { (x: Boolean) =>
  (!x, s"simple_1 called with $x\n")
}

val simple_2: Boolean => (Boolean, String) = { (x: Boolean) =>
  (!x, s"simple_2 called with $x\n")
}

def compose[A](f: A => (A, String), g: A => (A, String)): A => (A, String) =
  (a: A) => {
    val (a1, string1) = f(a)
    val (a2, string2) = g(a1)

    (a2, string1 + string2)
  }

def composed: Boolean => (Boolean, String) =
	compose(simple_1, simple_2)

scribe info s"Result:\n${composed(true)}"
```

Resulting in (manually formated for readability):

```bash
(true, simple_1 called with true
       simple_2 called with false
)
```

An identity function:

```scala
// Take an A and return a pair, it should do nothing to the log
def id[A](a: A): (A, String) = (a, "")
```

At this point we we have associativity and identity — in other words we have a **Category**.

In our Category the objects are Scala types and the arrows are `A` -> `(B, String)` (instead of simple `A` -> `B` arrows). The fact that the arrows are not simple transformation from `A` -> `B` makes them so called **Kleisli arrows**. Kleisli arrows can work with many different types, not only a tuple of some type `B` and `String`, they are defined for the types we impose as few conditions as possible, in other words for monadic types. This is where [Cats](https://typelevel.org/cats/datatypes/kleisli.html) definition of Kleisli comes in:

> Kleisli enables composition of functions that return a monadic value, for instance an `Option[Int]` or a `Either[String, List[Double]]`, without having functions take an `Option` or `Either` as a parameter, which can be strange and unwieldy.

Kleisli is all about composing. Let’s say we have 3 functions we want to compose:

```scala
val r = scala.util.Random

val generate: Unit => Int =
_ => r.nextInt(100)

val process: Int => Int =
_ * math.Pi toInt

val save: Int => Boolean =
_ => true
```

The simplest approach would be to call it one by one and pass the result from one to the other like so:

```scala
val generated: Int = generate()
val processed: Int = process(generated)
val saved: Boolean = save(processed)

scribe info s"Result is: $saved"
```

Resulting in:

```bash
Result is: true
```

Or, you could just inline those calls like so:

```scala
val combine: Unit => Boolean =
	_ => save(process(generate()))
```

In Scala we have functions like `compose` and `andThen` for doing just that:

```scala
val combine: Unit => Boolean =
	save compose process compose generate
```

```scala
val combine: Unit => Boolean =
	generate andThen process andThen save
```

The problem with all the above examples is that we need to match the  inputs of one function with the outputs of another to make this all  work. If some of the outputs will be a wrapper around some type (e.g.  Future or Cats IO) we will get into trouble quickly e.g.

```scala
val generate: Unit => IO[Int] =
	_ => IO.pure(r.nextInt(100))

val process: Int => IO[Double] =
	num => IO.pure(num * math.Pi)

val save: Double => IO[Boolean] =
	_ => IO.pure(true)
```

Now it’s clear that we cannot just pass return value from `generate` to `process` etc. But we know that IO is a Monad and we can easily flatMap on it:

```scala
val combine: Unit => Boolean =
	_ => {
  	val combine: Unit => IO[Boolean] =
  		_ => generate().flatMap { number =>
    		process(number).flatMap { processed =>
      		save(processed)
    		}
  		}

  	combine().unsafeRunSync()
	}
```

And using the more readable **for comprehension**:

```scala
val combine: Unit => Boolean =
	_ => {
  	val combine: Unit => IO[Boolean] =
  		_ => for {
    		number <- generate()
    		processed <- process(number)
    		result <- save(processed)
  		} yield result

  	combine().unsafeRunSync()
	}
```

**With Kleisli we can do something similar but in many cases in more clear and readable way**.

Let’s start with the most verbose version:

```scala
val combine: Kleisli[IO, Unit, Boolean] = {
  val generateK: Kleisli[IO, Unit, Int] = Kleisli(generate) // apply[F[_], -A, B](run: A => F[B])
  val processK: Kleisli[IO, Int, Double] = Kleisli(process)
  val saveK: Kleisli[IO, Double, Boolean] = Kleisli(save)

  generateK andThen processK andThen saveK
}

scribe info s"Result: ${combine.run(()).unsafeRunSync()}"
```

Again resulting in:

```bash
Result is: true
```

There is an operator in Cats for working with Kleisli which you can use to get much cleaner code:

```scala
import cats.arrow.Arrow.ops.toAllArrowOps

val combine: Kleisli[IO, Unit, Boolean] =
	Kleisli(generate) >>> Kleisli(process) >>> Kleisli(save)
```

But if sticking with **andThen**, we only have to wrap the first monadic function:

```scala
val combine: Kleisli[IO, Unit, Boolean] =
	Kleisli(generate) andThen process andThen save
```

