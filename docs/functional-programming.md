# Functional Programming

Functional programming in Scala is big on type classes. We'll often see two types:

```scala
trait Show[A] {
  def show(x: A): String
}

trait Functor[F[_]] {
  def fmap[A, B](fa: F[A])(f: A => B): F[B]
}
```
The second is interesting. The parametric type is a `higher kinded type`; in this case a `type constructor`.

Many of our type classes have a parametric type constructor, such as `F[_]`.
We could have a `List[_]`, which itself is a type constructor, which is just a function of type:
```scala
T => List[T]

// e.g.
// String => List[String]
// where shorthand for T => List[T] is:
List[_]
```
To provide a Tuple as a type constructor we would have e.g. a tuple 2:
```scala
Lambda[X => (X, X)]
```

The above is Scala 2. Scala 3 uses the following syntax:
```scala
[X] =>> (X, X)
```

All the above can be learnt from code and tests under package `tech.backwards.fp.learn`. You could view the `test`s first and try implementing before looking at the `main` code.
Go through in the following order:
- TypeClass
- TotalOrder
- Show
- Eq
- Monoid
- Functor
  - Id
  - Maybe
  - List
  - Disjunction
  - Writer
  - State
- Monad
  - Id
  - Maybe
  - List
  - Disjunction
  - Writer
  - State
- Foldable
- Applicative
  - Id
  - Maybe
  - List
  - Disjunction
  - Writer
  - State
- Traversal
- Nested
- ΙO
- MonadTransformer
  - IdT
  - MaybeT
  - DisjunctionT
  - WriterT
  - StateT
  - ReaderT
- Free
- ContravariantFunctor
  - Show
  - Predicate
- CoMonad
  - Id
  - Env
- Reader (Functor, Monad and Applicative)
- ReaderT
- Applicative for Monad Transformers
- Applicative for Free

At a high level, the following mentions a few functional concepts.

## Monad Transformer

The simplest is:
```scala
final case class IdT[F[_], A](value: F[Id[A]])
```
where another variation, specifically from the Cats library:
```scala
final case class IdT[F[_], A](value: F[A])
```

A monad transformer can be thought of as a more specific version of Nested, which is declared as:
```scala
final case class Nested[F[_], G[_], A](value: F[G[A]])
```

We can see that the Nested version is a type constructor `G` wrapped in another type constructor `F`.
Whereas, `IdT` is an (hard coded) `Id` type constructor wrapped in type constructor `F`.

Then of course there are other transformers such as, `OptionT`, `EitherT` etc.