package tech.backwards.tagless.withoutcats

/**
 * With Tagless Final, the first thing to do with this technique is to define our Algebras, our operations that are needed to solve our domain problem e.g.
 * {{{
 *   def getUser(..)
 *   def getAlgorithm(..)
 *   def executeAlgorithm(..)
 *   def filter(...)
 * }}}
 *
 * Note (below) we are defining our Types with Higher-Kinded Types parameters (F[_]) to abstract out the Container Structure that it is going to be use in each Interpreter.
 * A Higher-Kinded Type or Type Constructor is a Type which constructs a new Type based on a Type Parameter.
 * For example Option[+A] is a Type constructor which takes a Type, for example String and constructs the final Type, for example Option[String].
 * You can probe this in a Scala console with :kind command:
 * {{{
 *   scala> :k String
 *   String's kind is A
 *
 *   scala> :k Option
 *   Option's kind is F[+A]
 *
 *   scala> :k Option[String]
 *   Option[String]'s kind is A
 * }}}
 */
object algebras {
  import DataSource._

  ////////////////////////////////////////////////////////////////////////////////
  trait UserRepo[F[_]] {
    def getUser(userId: Option[Int]): F[UserId]
  }

  object UserRepo {
    def apply[F[_] : UserRepo]: UserRepo[F] = implicitly
  }

  ////////////////////////////////////////////////////////////////////////////////
  trait AlgorithmRepo[F[_]] {
    def getAlgorithm(recommenderId: Option[String]): F[Algorithm]

    def execute(algo: Algorithm, userId: UserId): F[UserRec]
  }

  object AlgorithmRepo {
    def apply[F[_] : AlgorithmRepo]: AlgorithmRepo[F] = implicitly
  }

  ////////////////////////////////////////////////////////////////////////////////
  trait Filter[F[_]] {
    def filter(userRec: UserRec, limit: Int): F[UserRec]
  }

  object Filter {
    def apply[F[_] : Filter]: Filter[F] = implicitly
  }

  ////////////////////////////////////////////////////////////////////////////////

  trait Program[F[_]] {
    def flatMap[A, B](fa: F[A], afb: A => F[B]): F[B]

    def map[A, B](fa: F[A], ab: A => B): F[B]

    def fold[A, B, C](fa: F[A], first: B => C, second: A => C): C
  }

  object Program {
    def apply[F[_] : Program]: Program[F] =
      implicitly
  }

  implicit class ProgramSyntax[F[_], A](fa: F[A]) {
    def map[B](f: A => B)(implicit Prog: Program[F]): F[B] =
      Prog.map(fa, f)

    def flatMap[B](afb: A => F[B])(implicit Prog: Program[F]): F[B] =
      Prog.flatMap(fa, afb)

    def fold[B, C](first: B => C, second: A => C)(implicit Prog: Program[F]): C =
      Prog.fold(fa, first, second)
  }

  // With all this machinery in place we can add some utility functions to avoid calling companion objects and just calling functions from the client program:
  def getUser[F[_] : UserRepo](userId: Option[Int]): F[UserId] =
    UserRepo[F].getUser(userId)

  def getAlgorithm[F[_] : AlgorithmRepo](recommenderId: Option[String]): F[Algorithm] =
    AlgorithmRepo[F].getAlgorithm(recommenderId)

  def execute[F[_] : AlgorithmRepo](algo: Algorithm, userId: UserId): F[UserRec] =
    AlgorithmRepo[F].execute(algo, userId)

  def filter[F[_] : Filter](userRec: UserRec, limit: Int): F[UserRec] =
    Filter[F].filter(userRec, limit)
}