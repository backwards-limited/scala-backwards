package com.backwards.fp.transaction

import cats._
import cats.effect._
import cats.syntax.all._

/**
 * [[https://ivan-kurchenko.medium.com/transactional-effect-in-scala-c40ef634b375 Transactional effect in Scala]]
 */
object TransactionEffectDemo extends IOApp {
  import TransactionEffect._

  override def run(args: List[String]): IO[ExitCode] = {
    for {
      _ <- IO.delay(println("A executed")).rollbackWith {
        case _: Throwable => IO.delay(println("A recovered"))
      }
      _ <- IO.delay(println("B executed")).rollbackWith {
        case _: Throwable => IO.delay(println("B recovered"))
      }
      _ <- IO.raiseError(new Exception("C failed"))
    } yield
      ExitCode.Success
  }
}

/**
 * Transactional effect provide possibility to recover effect execution result if it failed, but still return failed result.
 *
 * @tparam F surrounding effect type
 * @tparam T effect value type
 */
class TransactionEffect[F[_], E](
  underlying: F[E], rollback: PartialFunction[Throwable, F[Unit]]
)(implicit F: FlatMap[F], ME: MonadError[F, Throwable]) {
  def flatMap[S](f: E => F[S]): F[S] = {
    F.flatMap(underlying)(f).recoverWith {
      case exception: Throwable =>
        val failure: F[S] = ME.raiseError[S](exception)
        rollback.lift(exception).fold(failure)(recoverEffect => F.flatMap(recoverEffect)(_ => failure))
    }
  }
}

object TransactionEffect {
  /**
   * Provides syntax sugar over [[TransactionEffect]]
   */
  implicit class TransactionEffectSyntax[F[_], E](underling: F[E])(implicit F: FlatMap[F], AE: MonadError[F, Throwable]) {
    def rollbackWith(rollback: PartialFunction[Throwable, F[Unit]]): TransactionEffect[F, E] = {
      new TransactionEffect[F, E](underling, rollback)
    }
  }
}