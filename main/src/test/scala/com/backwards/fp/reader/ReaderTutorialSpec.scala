package com.backwards.fp.reader

import cats.data.ReaderT
import cats.effect.IO
import cats.effect.unsafe.implicits.global
import cats.implicits._
import cats.mtl.ApplicativeAsk
import cats.{Applicative, Monad}
import org.scalatest.matchers.must.Matchers
import org.scalatest.wordspec.AnyWordSpec

class ReaderTutorialSpec extends AnyWordSpec with Matchers {
  "Tutorial" should {
    /**
     * [[https://medium.com/@alexander.zaidel/no-more-readers-in-my-code-507247e76506]]
     *
     * - Get tweets
     * - Find the shortest one
     * - Get user’s information by userId from the shortest tweet
     */
    "start without MTL" in {
      final case class Environment(connectionPool: ConnectionPool, httpClient: HttpClient, config: Config)
      final case class Config(authToken: String)
      final case class User(id: Long, email: String)
      final case class Tweet(id: Long, userId: Long, content: String)

      sealed trait Connection {
        def execute(query: String): IO[Option[User]]
      }

      sealed trait ConnectionPool {
        def getConnection: Connection
      }

      sealed trait HttpClient {
        def get(url: String, token: String): IO[List[Tweet]]
      }

      sealed trait TwitterService {
        def getTweets: ReaderT[IO, Environment, List[Tweet]]

        def shortestTweet(tweets: List[Tweet]): IO[Option[Tweet]]
      }

      sealed trait UserRepository {
        def findUser(id: Long): ReaderT[IO, Environment, Option[User]]
      }

      // The implementation for the traits could be:
      object TwitterServiceStub extends TwitterService {
        private val GetTweetsUrl = "https://twitter.com/getTweets"

        override def getTweets: ReaderT[IO, Environment, List[Tweet]] = ReaderT { env =>
          env.httpClient.get(GetTweetsUrl, env.config.authToken)
        }

        override def shortestTweet(tweets: List[Tweet]): IO[Option[Tweet]] =
          tweets match {
            case Nil => IO.pure(None)
            case _ => IO.pure(Some(tweets.minBy(_.content)))
          }
      }

      object UserRepositoryStub extends UserRepository {
        def findUser(id: Long): ReaderT[IO, Environment, Option[User]] = ReaderT { env =>
          env.connectionPool.getConnection.execute(s"select * from user where id = $id")
        }
      }

      // The last part is to wire everything together:
      def getConnectionPool: ConnectionPool = ???
      def getHttpClient: HttpClient = ???
      def getConfig: Config = ???

      def shortestTweetAuthor: ReaderT[IO, Environment, Option[User]] = for {
        tweets <- TwitterServiceStub.getTweets
        shortestTweetOpt <- ReaderT.liftF(TwitterServiceStub.shortestTweet(tweets))
        shortestTweetAuthor <- shortestTweetOpt match {
          case Some(shortestTweet) => UserRepositoryStub.findUser(shortestTweet.userId)
          case None => ReaderT.liftF[IO, Environment, Option[User]](IO.pure(None))
        }
      } yield shortestTweetAuthor

      def program: Option[User] = {
        val env = Environment(getConnectionPool, getHttpClient, getConfig)
        shortestTweetAuthor.run(env).unsafeRunSync()
      }

      // Overall it isn’t much different from a regular implementation,
      // but ReaderT is everywhere and everything that is not ReaderT has to be lifted.
      // Could it be worse? Sure, more monad transformers:
    }

    "introduce MTL" in {
      final case class Environment(connectionPool: ConnectionPool, httpClient: HttpClient, config: Config)
      final case class Config(authToken: String)
      final case class User(id: Long, email: String)
      final case class Tweet(id: Long, userId: Long, content: String)

      sealed trait Connection {
        def execute[F[_]](query: String): F[Option[User]]
      }

      sealed trait ConnectionPool {
        def getConnection: Connection
      }

      sealed trait HttpClient {
        def get[F[_]](url: String, token: String): F[List[Tweet]]
      }

      // cats-mtl, which provides a typeclass ApplicativeAsk.
      /*
      // E - environment of dependencies, in our example it’s Environment
      trait ApplicativeAsk[F[_], E] {
        val applicative: Applicative[F]

        // Even though there are no restrictions on F, we have to provide and instance of Applicative[F], so we will be able to implement ask by lifting the E into F[E].
        def ask: F[E]

        // A function that provides a specific (specific and lifted) dependency, out of environment E.
        // E.g. httpClient from Environment:
        // applicativeAsk.reader(env => env.httpClient)
        def reader[A](f: E => A): F[A]
      }
      */

      // Let’s implement the same shortest tweet task using final tagless over F[_]:
      trait TwitterServiceAlg[F[_]] {
        def getTweets: F[List[Tweet]]

        def shortestTweet(tweets: List[Tweet]): F[Option[Tweet]]
      }

      trait UserRepositoryAlg[F[_]] {
        def findUser(id: Long): F[Option[User]]
      }

      // Time to think about F, what effects should it bring to the table?
      // It should be a Monad, because of sequential computation of the provided algebra and ApplicativeAsk for dependency injection.

      // Since our dependencies are fixed over Environment, we can introduce a type alias:
      type EnvironmentAsk[F[_]] = ApplicativeAsk[F, Environment]

      class TwitterServiceAlgM[F[_]: EnvironmentAsk: Monad] extends TwitterServiceAlg[F] {
        private val GetTweetsUrl = "https://twitter.com/getTweets"
        private val env = implicitly[EnvironmentAsk[F]]

        override def getTweets: F[List[Tweet]] = for {
          httpClient <- env.reader(_.httpClient)
          authToken <- env.reader(_.config.authToken)
          tweets <- httpClient.get[F](GetTweetsUrl, authToken)
        } yield tweets

        override def shortestTweet(tweets: List[Tweet]): F[Option[Tweet]] =
          tweets match {
            case Nil => Monad[F].pure(None)
            case _ => Monad[F].pure(Some(tweets.minBy(_.content)))
          }
      }

      class UserRepositoryAlgM[F[_]: EnvironmentAsk: Monad] extends UserRepositoryAlg[F] {
        private val env = implicitly[EnvironmentAsk[F]]

        override def findUser(id: Long): F[Option[User]] = for {
          connectionPool <- env.reader(_.connectionPool)
          user <- connectionPool.getConnection.execute[F](s"select * from user where id = $id")
        } yield user
      }

      def shortestTweetAuthor[F[_]: EnvironmentAsk: Monad](
        userRepositoryAlg: UserRepositoryAlg[F],
        twitterServiceAlg: TwitterServiceAlg[F]
      ): F[Option[User]] =
        for {
          tweets <- twitterServiceAlg.getTweets
          shortestTweetOpt <- twitterServiceAlg.shortestTweet(tweets)
          shortestTweetAuthor <- shortestTweetOpt match {
            case Some(shortestTweet) => userRepositoryAlg.findUser(shortestTweet.userId)
            case None => Monad[F].pure(None)
          }
        } yield shortestTweetAuthor

      // And it’s time to decide what our F[_] will be. Assuming that F[_] is IO, ApplicativeAsk can be implemented this way:
      class EnvironmentIOAsk(env: Environment) extends ApplicativeAsk[IO, Environment] {
        override val applicative: Applicative[IO] = Applicative[IO]

        override def ask: IO[Environment] = applicative.pure(env)

        override def reader[A](f: Environment => A): IO[A] = ask.map(f)
      }


      def getConnectionPool: ConnectionPool = ???
      def getHttpClient: HttpClient = ???
      def getConfig: Config = ???

      def program: Option[User] = {
        implicit val configEnv: EnvironmentIOAsk =
          new EnvironmentIOAsk(Environment(getConnectionPool, getHttpClient, getConfig))

        val userRepositoryAlg = new UserRepositoryAlgM
        val twitterServiceAlg = new TwitterServiceAlgM

        shortestTweetAuthor[IO](userRepositoryAlg, twitterServiceAlg).unsafeRunSync()
      }
    }
  }
}