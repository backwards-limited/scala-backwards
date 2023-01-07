package tech.backwards.bookofmonads.ch4

import cats.implicits.{catsSyntaxOptionId, none}
import weaver._

/**
 * Traversable
 */
object Ex2Suite extends SimpleIOSuite {
  /**
   * The list constructor supports two ways of mapping a function over all of its elements:
   * {{{
   *   Purely, via
   *   map :: (a -> b) -> [a] -> [b]
   *
   *   Monadically, via
   *   mapM :: Monad m => (a -> m b) -> [a] -> m [b]
   * }}}
   * The first notion can be extended to other type constructors, giving rise to the Functor type class.
   * We can do the same monadically with Traversable.
   *
   * The comparison of the implementations for "map" and "mapM" are shown in this Haskell equivalent: Ch4.Ex2Spec
   *
   * Traverse (or Traverable in Haskell) has the function "traverse" which is essentially "mapM",
   * except "mapM" requests a full "Monad" whereas "traverse" requests just "Applicative".
   * {{{
   *   class Functor f => Traversable f where
   *      traverse :: Applicative m => (a -> m b) -> f a -> m (f b)
   * }}}
   */
  pureTest("Traverse")(
    expect(true)
  )
}