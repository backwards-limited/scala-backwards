package com.backwards.tagless.withoutcats

import scala.util.Random

object DataSource {
  sealed abstract class AppError(val message: String) extends Throwable

  final case object UnknownError extends AppError(s"Unexpected Error")

  final case class UserNotFound(userId: UserId) extends AppError(s"User not found for id $userId")

  final case object UserNotProvided extends AppError(s"User id must be provided")

  final case class AlgorithmNotFound(recId: String) extends AppError(s"Algorithm not found for id $recId")

  final case class RecommendationsNotFound(userId: UserId, algo: String) extends AppError(s"Recommendations not found for $userId with algorithm '$algo'")

  final case class UserId(userId: Int) extends AnyVal

  final case class Rec(recId: String, score: Float)

  final case class UserRec(userId: UserId, recs: List[Rec])

  final case class Result(algorithm: Algorithm, recs: UserRec)

  lazy val users: List[UserId] =
    (1 until 10).map(UserId).toList

  lazy val recs: IndexedSeq[Rec] =
    ('a' to 'z').map(c => Rec(c.toString, Random.nextFloat))

  lazy val recommendations: List[UserRec] =
    users.map(user =>
      if (user.userId % 2 == 0) UserRec(user, List.empty)
      else UserRec(user, recs.toList)
    )

  def emptyRecs(user: Int): UserRec =
    UserRec(UserId(user), List.empty)

  final case class Algorithm(name: String, run: UserId => Option[UserRec])

  val algo1: Algorithm =
    Algorithm("algo1", userId => recommendations.find(_.userId == userId))

  val algo2: Algorithm =
    Algorithm(
      "algo2",
      userId => recommendations
        .find(_.userId == userId)
        .map(_.copy(recs = recs.filter(_.recId > "h").toList))
    )

  val algo3: Algorithm =
    Algorithm("algo3", _ => None)

  lazy val algorithms: Map[String, Algorithm] =
    Map(
      "algo1" -> algo1,
      "algo2" -> algo2,
      "algo3" -> algo3
    )

  val algoDefault: Option[String] =
    Some("algo1")

  val limitDefault: Int = 10
}