package tech.backwards.tagless.withoutcats

import munit.FunSuite

/**
 * [[https://jproyo.github.io/posts/2019-02-07-practical-tagless-final-in-scala.html Tagless Final in Scala: A Practical example]]
 *
 * We are going to describe a Recommender Program that given an algorithm and a user is going to generate recommendations for that user based on the selected algorithm.
 * A Recommender system could be for example the most likely recommendations you received when visit any Marketplace such as Amazon, Ebay, or any other after searching for some products and navigate through these sites.
 * For example: “Since you have been searching for X cars, maybe you are interested in R, Y and Z also”.
 *
 * We have the following requirements defined in terms of User Stories:
 *  - As an user i want to get recommendations from an specific algorithm, but if there are no recommendations for this algorithm or i forgot to specify what algorithm should be use i would like to have default recommendations from the best algorithm the system has.
 *  - As an user i want to get a message if recommendation’s algorithm i requested is wrong.
 *  - As an user i want to be able to be retrieve with a limited number of recommendations.
 */
class TaglessWithoutCatsSuite extends FunSuite {
  test("hi") {
    import cats.implicits._

    assert(1 === 1)
  }

  test("imperative approach without tageless final") {
    import tech.backwards.tagless.withoutcats.DataSource._

    def getUser(userId: Option[Int]): Option[Int] =
      userId.filter(user => users.exists(_.userId == user))

    def getAlgorithm(recommenderId: Option[String]): Option[Algorithm] =
      recommenderId orElse algoDefault flatMap algorithms.get

    def program(
      userId: Option[Int],
      recommenderId: Option[String] = None,
      limit: Option[Int] = None
    ): Unit = {
      val user: Option[Int] =
        getUser(userId)

      val algorithm: Option[Algorithm] =
        getAlgorithm(recommenderId)

      val result: Option[UserRec] =
        user.flatMap(userId =>
          algorithm.flatMap(_.run(UserId(userId))) orElse Some(emptyRecs(userId))
        )

      val limitFilter: Int =
        limit getOrElse limitDefault

      val resultFiltered: Option[UserRec] =
        result.map(_.copy(recs = recs.slice(0, limitFilter).toList))

      resultFiltered match {
        case Some(recs) =>
          println(s"\nRecommnedations for userId ${recs.userId}...")
          println(s"Algorithm ${algorithm.get.name}")
          println(s"Recs: ${recs.recs}")

        case None =>
          println(s"No recommendations found for userId $userId")
      }
    }
  }

  test("improve imperative approach without tageless final") {
    import tech.backwards.tagless.withoutcats.DataSource._

    def getUser(userId: Option[Int]): Option[Int] =
      userId.filter(user => users.exists(_.userId == user))

    def getAlgorithm(recommenderId: Option[String]): Option[Algorithm] =
      recommenderId orElse algoDefault flatMap algorithms.get

    def program(
      userId: Option[Int],
      recommenderId: Option[String] = None,
      limit: Option[Int] = None
    ): Unit = {
      val result = for {
        user           <- getUser(userId)
        algorithm      <- getAlgorithm(recommenderId)
        result         <- algorithm run UserId(user)
        limitFilter     = limit getOrElse limitDefault
        resultFiltered  = result.copy(recs = recs.slice(0, limitFilter).toList)
      } yield
        Result(algorithm, resultFiltered)

      result match {
        case Some(algoRes) =>
          println(s"\nRecommnedations for userId ${algoRes.recs.userId}...")
          println(s"Algorithm ${algoRes.algorithm.name}")
          println(s"Recs: ${algoRes.recs.recs}")

        case None =>
          println(s"No recommendations found for userId $userId")
      }
    }
  }

  test("improve more imperative approach without tageless final") {
    import tech.backwards.tagless.withoutcats.DataSource._

    def getUser(userId: Option[Int]): Option[Int] =
      userId.filter(user => users.exists(_.userId == user))

    def getAlgorithm(recommenderId: Option[String]): Option[Algorithm] =
      recommenderId orElse algoDefault flatMap algorithms.get

    def executeAlgorithm(user: UserId, algorithm: Algorithm): Option[UserRec] =
      algorithm.run(user)

    def filterResults(result: UserRec, limitFilter: Int): Option[UserRec] =
      Some(result.copy(recs = recs.slice(0, limitFilter).toList))

    def getRecommendations(
      userId: Option[Int],
      recommenderId: Option[String],
      limit: Option[Int]
    ): Option[Result] =
      for {
        user           <- getUser(userId)
        algorithm      <- getAlgorithm(recommenderId)
        result         <- executeAlgorithm(UserId(user), algorithm)
        limitFilter     = limit getOrElse limitDefault
        resultFiltered <- filterResults(result, limitFilter)
      } yield
        Result(algorithm, resultFiltered)

    def printResults(userId: Option[Int], result: Option[Result]): Unit =
      result.fold(println(s"No recommendations found for userId $userId")) { algoRes =>
        println(s"\nRecommnedations for userId ${algoRes.recs.userId}...")
        println(s"Algorithm ${algoRes.algorithm.name}")
        println(s"Recs: ${algoRes.recs.recs}")
      }

    def program(
      userId: Option[Int],
      recommenderId: Option[String] = None,
      limit: Option[Int] = None
    ): Unit = {
      val results: Option[Result] =
        getRecommendations(userId, recommenderId, limit)

      printResults(userId, results)
    }
  }

  /**
   * We can say that in Tagless Final style there are:
   *  - Algebras: Set of operations over a Structure. In a Programming language idiom could be a set of functions that operates over some Type.
   *  - Interpreter: The way of those operations behave according to an specific Type. In a Programming language idiom the implementation of those functions depending on the specific Type.
   */
  test("tagless final") {
    import tech.backwards.tagless.withoutcats.algebras._
    import tech.backwards.tagless.withoutcats.DataSource._

    def printResults[F[_]: Program](result: F[Result]): Unit =
      result.fold[AppError, Unit](error => println(s"Error ${error.message}"), algoRes => {
        println(s"\nRecommnedations for userId ${algoRes.recs.userId}...")
        println(s"Algorithm ${algoRes.algorithm.name}")
        println(s"Recs: ${algoRes.recs.recs}")
      })

    def getRecommendations[F[_]: UserRepo: AlgorithmRepo: Filter: Program](
      userId: Option[Int],
      recommenderId: Option[String],
      limit: Option[Int]
    ): F[Result] =
      for {
        user           <- getUser(userId)
        algorithm      <- getAlgorithm(recommenderId)
        result         <- execute(algorithm, user)
        limitFilter     = limit.getOrElse(limitDefault)
        resultFiltered <- filter(result, limitFilter)
      } yield
        Result(algorithm, resultFiltered)

    def testOption(): Unit = {
      import tech.backwards.tagless.withoutcats.interpreters.option._

      val result: Option[Result] =
        getRecommendations[Option](userId = Some(1), recommenderId = Some("no-algo"), limit = Some(5))

      printResults(result)
    }

    def testEither(): Unit = {
      import tech.backwards.tagless.withoutcats.interpreters.either._

      val result: Either[AppError, Result] =
        getRecommendations[Either[AppError, *]](userId = Some(1), recommenderId = Some("no-algo"), limit = Some(5))

      printResults(result)


      val result2: Either[AppError, Result] =
        getRecommendations[Either[AppError, *]](userId = Some(1), recommenderId = Some("algo1"), limit = Some(3))

      printResults(result2)
    }

    testEither()
  }
}