package tech.backwards.fp.learn

abstract class Traversal[F[_]] {
  def traverse[G[_]: Applicative, A, B](fa: F[A])(f: A => G[B]): G[F[B]]
}

object Traversal extends TraversalImplicits {
  def apply[F[_]: Traversal]: Traversal[F] =
    implicitly

  object syntax {
    implicit class TraversalSyntax[F[_]: Traversal, A](fa: F[A]) {
      def traverse[G[_]: Applicative, B](f: A => G[B]): G[F[B]] =
        apply[F].traverse(fa)(f)
    }

    implicit class TraversalSequenceSyntax[F[_]: Traversal, G[_]: Applicative, A](fa: F[G[A]]) {
      def sequence: G[F[A]] =
        apply[F].traverse(fa)(identity)
    }

    implicit class TraversalTuple2Syntax[A](fa: (A, A)) {
      def traverse[G[_]: Applicative, B](f: A => G[B]): G[(B, B)] =
        traversalTuple2.traverse(fa)(f)
    }

    implicit class TraversalTuple2SequenceSyntax[F[_]: Applicative, A](fa: (F[A], F[A])) {
      def sequence: F[(A, A)] = {
        import tech.backwards.fp.learn.Applicative.syntax.function._

        Applicative[F].functor.fmap(fa._1)((x: A) => (y: A) => (x, y)).ap(fa._2)
      }
    }

    implicit class TraversalTuple3Syntax[A](fa: (A, A, A)) {
      def traverse[G[_]: Applicative, B](f: A => G[B]): G[(B, B, B)] =
        traversalTuple3.traverse(fa)(f)
    }

    implicit class TraversalTuple3SequenceSyntax[F[_]: Applicative, A](fa: (F[A], F[A], F[A])) {
      def sequence: F[(A, A, A)] = {
        import tech.backwards.fp.learn.Applicative.syntax.function._

        Applicative[F].functor.fmap(fa._1)((x: A) => (y: A) => (z: A) => (x, y, z)).ap(fa._2).ap(fa._3)
      }
    }
  }
}

sealed trait TraversalImplicits {
  implicit val traversalTuple2: Traversal[Lambda[X => (X, X)]] =
    new Traversal[Lambda[X => (X, X)]] {
      def traverse[G[_]: Applicative, A, B](fa: (A, A))(f: A => G[B]): G[(B, B)] = {
        import tech.backwards.fp.learn.Applicative.syntax.function._

        Applicative[G].functor.fmap(f(fa._1))((x: B) => (y: B) => (x, y)) ap f(fa._2)
      }

      /*
      Original, when Applicative did not declare a Functor instance,
      and Traversal had the declaration: def traverse[G[_]: Applicative: Functor, A, B]

      def traverse[G[_] : Functor : Applicative, A, B](fa: (A, A))(f: A => G[B]): G[(B, B)] = {
        import tech.backwards.fp.learn.Applicative.syntax.function._
        import tech.backwards.fp.learn.Functor.syntax.function._

        ((x: B) => (y: B) => (x, y)) `<$>` f(fa._1) <*> f(fa._2)
      }
      */
    }

  implicit val traversalTuple3: Traversal[Lambda[X => (X, X, X)]] =
    new Traversal[Lambda[X => (X, X, X)]] {
      def traverse[G[_]: Applicative, A, B](fa: (A, A, A))(f: A => G[B]): G[(B, B, B)] = {
        import tech.backwards.fp.learn.Applicative.syntax.function._

        Applicative[G].functor.fmap(f(fa._1))((x: B) => (y: B) => (z: B) => (x, y, z)) ap f(fa._2) ap f(fa._3)
      }

      /*
      Original, when Applicative did not declare a Functor instance,
      and Traversal had the declaration: def traverse[G[_]: Applicative: Functor, A, B]

      def traverse[G[_] : Applicative : Functor, A, B](fa: (A, A, A))(f: A => G[B]): G[(B, B, B)] = {
        import tech.backwards.fp.learn.Applicative.syntax.function._
        import tech.backwards.fp.learn.Functor.syntax.function._

        ((x: B) => (y: B) => (z: B) => (x, y, z)) `<$>` f(fa._1) <*> f(fa._2) <*> f(fa._3)
      }
      */
    }

  implicit val traversalList: Traversal[List] =
    new Traversal[List] {
      def traverse[G[_]: Applicative, A, B](fa: List[A])(f: A => G[B]): G[List[B]] = {
        import tech.backwards.fp.learn.Applicative.syntax.function._

        fa.foldRight(Applicative[G].pure(List.empty[B])) { (a, bs) =>
          Applicative[G].functor.fmap(f(a))((b: B) => (bs: List[B]) => b :: bs) ap bs
        }
      }
    }
}