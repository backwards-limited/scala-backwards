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

All the above can be learnt from code and tests under package `tech.backwards.fp.learn`. Go through in the following order:
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
  - WriterT
  - StateT
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