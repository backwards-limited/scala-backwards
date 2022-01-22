package tech.backwards.fp.transaction

import cats._
import cats.effect._
import cats.syntax.all._

/**
 * [[https://ivan-kurchenko.medium.com/transactional-effect-in-scala-c40ef634b375 Transactional effect in Scala]]
 */
object TransactionEffectDemo extends IOApp {
  import TransactionEffect._

  override def run(args: List[String]): IO[ExitCode] =
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

/**
 * Transactional effect provide possibility to recover effect execution result if it failed, but still return failed result.
 *
 * @tparam F surrounding effect type
 * @tparam A effect value type
 */
class TransactionEffect[F[_]: FlatMap: MonadError[*[_], Throwable], A](underlying: F[A], rollback: PartialFunction[Throwable, F[Unit]]) {
  def flatMap[B](f: A => F[B]): F[B] =
    FlatMap[F].flatMap(underlying)(f).recoverWith {
      case exception: Throwable =>
        val failure: F[B] =
          MonadError[F, Throwable].raiseError[B](exception)

        rollback.lift(exception).fold(failure)(recoverEffect => FlatMap[F].flatMap(recoverEffect)(_ => failure))
    }
}

object TransactionEffect {
  /**
   * Provides syntax sugar over [[TransactionEffect]]
   */
  implicit class TransactionEffectSyntax[F[_]: FlatMap: MonadError[*[_], Throwable], A](underling: F[A]) {
    def rollbackWith(rollback: PartialFunction[Throwable, F[Unit]]): TransactionEffect[F, A] =
      new TransactionEffect[F, A](underling, rollback)
  }
}