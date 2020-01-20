package com.backwards.cats.monad

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

}