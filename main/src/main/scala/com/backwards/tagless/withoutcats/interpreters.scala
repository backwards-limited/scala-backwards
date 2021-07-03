package com.backwards.tagless.withoutcats

object interpreters {
  import algebras._
  import DataSource._

  object option {
    implicit object UserRepoOption extends UserRepo[Option] {
      override def getUser(userId: Option[Int]): Option[UserId] =
        userId.filter(user => users.exists(_.userId == user)).map(UserId)
    }

    implicit object AlgorithmRepoOption extends AlgorithmRepo[Option]{
      override def getAlgorithm(recommenderId: Option[String]): Option[Algorithm] =
        recommenderId orElse algoDefault flatMap algorithms.get

      override def execute(algo: Algorithm, userId: UserId): Option[UserRec] = algo run userId
    }

    implicit object FilterOption extends Filter[Option] {
      override def filter(userRec: UserRec, limit: Int): Option[UserRec] =
        Some(userRec.copy(recs = recs.slice(0, limit).toList))
    }

    implicit object ProgramOption extends Program[Option] {
      override def flatMap[A, B](fa: Option[A], afb: A => Option[B]): Option[B] = fa flatMap afb

      override def map[A, B](fa: Option[A], ab: A => B): Option[B] = fa map ab

      override def fold[A, B, C](fa: Option[A], first: B => C, second: A => C): C =
        fa.fold(first(UnknownError.asInstanceOf[B]))(second(_))
    }
  }

  /**
   * Using Option as the Higher Kinded Type for our interpreter was easy because Option's kind matches that of our algebra.
   * However, Either has 2 types instead of the desired 1.
   * To solve this we make use of Lambda Type.
   *
   * Basically a Lambda Type is similar to a Partially Applied function but at the Type level.
   * We can curry our 2 parameter Type Constructor to obtain another 1 parameter Type Constructor:
   * {{{
   *  scala-backwards on  master
   *  ➜ scala
   *
   *  scala> :kind ({ type T[A] = Either[Throwable, A] })#T
   *  scala.util.Either[Throwable,?]'s kind is F[+A]
   *
   *  scala> sealed abstract class AppError(val message: String) extends Throwable
   *  class AppError
   *
   *  scala> :kind ({ type T[A] = Either[AppError, A] })#T
   *  scala.util.Either[AppError,?]'s kind is F[+A]
   * }}}
   *
   * As we can see in Scala console example we are fixing Left Either parameter type with AppError since all errors we are going to generate are subtypes of this,
   * and let this phantom type be parameterized only in its Right value Type which is the Type it is going to change during execution.
   *
   * With kind-projector we can have a more readable Lambda Type like this:
   * {{{
   *  scala> :kind Either[AppError, *]
   *  scala.util.Either[AppError,?]'s kind is F[+A]
   * }}}
   *
   * or (with a kind projector ScalaOption setting):
   * {{{
   *  scala> :kind Either[AppError, _]
   *  scala.util.Either[AppError,?]'s kind is F[+A]
   * }}}
   */
  object either {
    implicit object UserRepoEither extends UserRepo[Either[AppError, *]] {
      override def getUser(userId: Option[Int]): Either[AppError, UserId] =
        for {
          userParam <- userId.map(UserId).toRight(UserNotProvided)
          userDb    <- users.find(_ == userParam).toRight(UserNotFound(userParam))
        } yield userDb
    }

    implicit object AlgorithmRepoEither extends AlgorithmRepo[Either[AppError, *]]{
      override def getAlgorithm(recommenderId: Option[String]): Either[AppError, Algorithm] =
        recommenderId.orElse(algoDefault).flatMap(algorithms.get)
          .toRight(AlgorithmNotFound(recommenderId.getOrElse(algoDefault.get)))

      override def execute(algo: Algorithm, userId: UserId): Either[AppError, UserRec] =
        algo.run(userId).toRight(RecommendationsNotFound(userId, algo.name))
    }

    implicit object FilterEither extends Filter[Either[AppError, *]] {
      override def filter(userRec: UserRec, limit: Int): Either[AppError, UserRec] =
        Right(userRec.copy(recs = recs.slice(0, limit).toList))
    }

    implicit object ProgramEither extends Program[Either[AppError, *]] {
      override def flatMap[A, B](fa: Either[AppError, A], afb: A => Either[AppError, B]): Either[AppError, B] =
        fa.flatMap(afb)

      override def map[A, B](fa: Either[AppError, A], ab: A => B): Either[AppError, B] =
        fa.map(ab)

      override def fold[A, B, C](fa: Either[AppError, A], first: B => C, second: A => C): C =
        fa.fold(error => first(error.asInstanceOf[B]), second(_))
    }
  }
}