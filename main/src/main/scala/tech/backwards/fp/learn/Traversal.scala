package tech.backwards.fp.learn

abstract class Traversal[F[_]] {
  def traverse[G[_]: Functor: Applicative, A, B](fa: F[A])(f: A => G[B]): G[F[B]]
}

object Traversal extends TraversalImplicits {
  def apply[F[_]: Traversal]: Traversal[F] =
    implicitly

  object syntax {
    implicit class TraversalSyntax[F[_]: Traversal, A](fa: F[A]) {
      def traverse[G[_]: Functor: Applicative, B](f: A => G[B]): G[F[B]] =
        apply[F].traverse(fa)(f)
    }

    implicit class TraversalSequenceSyntax[F[_]: Traversal, G[_]: Functor: Applicative, A](fa: F[G[A]]) {
      def sequence: G[F[A]] =
        apply[F].traverse(fa)(identity)
    }

    implicit class TraversalTuple2Syntax[A](fa: (A, A)) {
      def traverse[G[_]: Functor: Applicative, B](f: A => G[B]): G[(B, B)] =
        traversalTuple2.traverse(fa)(f)
    }

    implicit class TraversalTuple3Syntax[A](fa: (A, A, A)) {
      def traverse[G[_]: Functor: Applicative, B](f: A => G[B]): G[(B, B, B)] =
        traversalTuple3.traverse(fa)(f)
    }
  }
}

sealed trait TraversalImplicits {
  implicit val traversalTuple2: Traversal[Lambda[X => (X, X)]] =
    new Traversal[Lambda[X => (X, X)]] {
      def traverse[G[_]: Functor: Applicative, A, B](fa: (A, A))(f: A => G[B]): G[(B, B)] = {
        import tech.backwards.fp.learn.Applicative.syntax.function._
        import tech.backwards.fp.learn.Functor.syntax.function._

        ((x: B) => (y: B) => (x, y)) `<$>` f(fa._1) <*> f(fa._2)
      }
    }

  implicit val traversalTuple3: Traversal[Lambda[X => (X, X, X)]] =
    new Traversal[Lambda[X => (X, X, X)]] {
      def traverse[G[_]: Functor: Applicative, A, B](fa: (A, A, A))(f: A => G[B]): G[(B, B, B)] = {
        import tech.backwards.fp.learn.Applicative.syntax.function._
        import tech.backwards.fp.learn.Functor.syntax.function._

        ((x: B) => (y: B) => (z: B) => (x, y, z)) `<$>` f(fa._1) <*> f(fa._2) <*> f(fa._3)
      }
    }
}