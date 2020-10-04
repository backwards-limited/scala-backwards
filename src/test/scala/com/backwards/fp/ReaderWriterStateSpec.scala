package com.backwards.fp

import cats.Monad
import cats.data.ReaderWriterStateT
import cats.effect.IO
import org.scalatest.matchers.must.Matchers
import org.scalatest.wordspec.AnyWordSpec
import cats.implicits._
import cats.mtl.{ApplicativeAsk, FunctorTell, MonadState}
import org.scalamock.scalatest.MockFactory

class ReaderWriterStateSpec extends AnyWordSpec with Matchers with MockFactory {
  "Reader Writer State" should {
    "be applied with Cats IndexedReaderWriterStateT" in {
      /*
      Represents a stateful computation in a context `F[_]`, over state `S`, with an
      initial environment `E`, an accumulated log `L` and a result `A`:

      type ReaderWriterStateT[F[_], E, L, S, A] = IndexedReaderWriterStateT[F, E, L, S, S, A]

      To get a good feeling how ReaderWriterStateT works, why don’t we implement a function getUser(uuid: String): IO[Option[User]],
      which makes an http call, stores result into a cache with a bit of logging.

      Cache will be an immutable data structure:
      */
      final case class Cache[K, V](data: Map[K, V]) {
        def put(key: K, value: V): Cache[K, V] = Cache(data + (key -> value))

        def get(key: K): Option[V] = data.get(key)

        def remove(key: K): Cache[K, V] = Cache(data - key)
      }

      /*
      In this example:
      - context of computation is IO
      - environment is Environment
      - log is Log (type alias for List[String])
      - state is Cache[UUID, User]
      - result of computation is Option[User]

      giving us:
      */
      type UUID = String

      final case class Config(userUuids: List[UUID])
      final case class Environment(httpClient: HttpClient, config: Config)
      final case class User()

      sealed trait HttpClient {
        def get(url: String)(id: UUID): IO[Option[User]]
      }

      type Log = List[String]
      // ReaderWriterStateT[IO, Environment, Log, Cache[UUID, User], Option[User]]

      // One of the ways it can be constructed is by using the function:
      // val f: (Environment, Cache[UUID, User]) => IO[(Log, Cache[UUID, User], Option[User])] = ???

      // The getUser could then be:
      def getUser(uuid: UUID): ReaderWriterStateT[IO, Environment, Log, Cache[UUID, User], Option[User]] =
        ReaderWriterStateT[IO, Environment, Log, Cache[UUID, User], Option[User]] {
          case (env, cache) =>
            val r: IO[(Log, Cache[UUID, User], Option[User])] =
              env.httpClient.get("Uri")(uuid).map {
                case Some(user) =>
                  (List(s"Found a user $user"), cache.put(uuid, user), Some(user))
                case None =>
                  (List(s"User with id $uuid doest not exist in the system"), cache, None)
              }

            r
        }

      // Usage:
      class HttpClientStub extends HttpClient {
        def get(url: String)(id: UUID): IO[Option[User]] = IO(None)
      }

      val result = for {
        _ <- getUser("user1")
        _ <- getUser("user2")
      } yield ()

      val (logs, userCache, _) = result
        .run(Environment(new HttpClientStub, Config(Nil)), Cache(Map.empty))
        .unsafeRunSync()



      def preCacheUsers(userUuids: List[UUID]): ReaderWriterStateT[IO, Environment, Log, Cache[UUID, User], Unit] =
        userUuids match {
          case Nil =>
            ReaderWriterStateT.pure[IO, Environment, Log, Cache[UUID, User], Unit]()

          case currentUserUuid :: rest =>
            for {
              _ <- ReaderWriterStateT.tell[IO, Environment, Log, Cache[UUID, User]](List(s"Pre caching user $currentUserUuid"))
              _ <- getUser(currentUserUuid)
              result <- preCacheUsers(rest)
            } yield result
        }

      // Such function can be used as:
      val result2 = for {
        environment <- ReaderWriterStateT.ask[IO, Environment, List[String], Cache[UUID, User]]
        _ <- preCacheUsers(environment.config.userUuids)
      } yield ()

      val (logs2, newState2, _) = result2
        .run(Environment(new HttpClientStub, Config(List("user1", "user2"))), Cache(Map.empty))
        .unsafeRunSync()
    }

    "applied with MTL" in {
      final case class Config(userUuids: List[UUID])
      final case class Environment(httpClient: HttpClient, config: Config)
      final case class User()

      sealed trait HttpClient {
        def get[F[_]](url: String)(id: UUID): F[Option[User]]
      }

      final case class Cache[K, V](data: Map[K, V]) {
        def put(key: K, value: V): Cache[K, V] = Cache(data + (key -> value))

        def get(key: K): Option[V] = data.get(key)

        def remove(key: K): Cache[K, V] = Cache(data - key)
      }

      // Type aliases for convenience:
      type UUID = String
      type Log = List[String]

      type Env[F[_]] = ApplicativeAsk[F, Environment] // Dependency injection
      type Logging[F[_]] = FunctorTell[F, Log] // Logging
      type State[F[_]] = MonadState[F, Cache[UUID, User]] // State management

      // Time to implement getUser(uuid: UUID): F[Option[User]] which will make an http call to find a user and populate cache if case of the user was found:
      class UserService[F[_] : Logging : Env : State : Monad] {
        private val URI = "http://someurl.com"

        private val env = implicitly[Env[F]]
        private val logging = implicitly[Logging[F]]
        private val state = implicitly[State[F]]
        private val M = implicitly[Monad[F]]

        def getUser(uuid: UUID): F[Option[User]] = for {
          httpClient <- env.reader(_.httpClient)
          user <- httpClient.get[F](URI)(uuid)
          _ <- user match {
            case Some(user) =>
              for {
                cache <- state.get
                _ <- logging.tell(List(s"Found user $user"))
                _ <- state.set(cache.put(uuid, user))
              } yield ()
            case None =>
              logging.tell(List(s"User with uuid $uuid doest not exist in the system"))
          }
        } yield user

        // Once getUser function is implemented, preCacheUsers which will iterate over a list of userUuids and call getUser to populate the cache would look like:
        def preCacheUsers(userUuids: List[UUID]): F[Unit] =
          userUuids match {
            case Nil =>
              M.pure(())
            case currentUserUuid :: rest =>
              for {
                _ <- logging.tell(List(s"Pre caching user $currentUserUuid"))
                _ <- getUser(currentUserUuid)
                res <- preCacheUsers(rest)
              } yield res
          }
      }

      // The last step is to “commit to a monad”, which will be ReaderWriterStateT:
      object ProgramExample1 {
        import cats.effect.IO
        import cats.implicits._
        import cats.mtl.implicits._

        def program: Unit = {
          val env = Environment(mock[HttpClient], Config(Nil))

          type RWS[T] = ReaderWriterStateT[IO, Environment, List[String], Cache[String, User], T]

          val service = new UserService[RWS]

          val (cache, log, _) = service.preCacheUsers(env.config.userUuids)
            .run(env, Cache(Map.empty)) // run ReaderWriterStateT
            .unsafeRunSync()
        }
      }

      /* TODO - Not compiling
      object ProgramExample2 {
        import cats.effect.IO
        import cats.implicits._
        import cats.mtl.implicits._
        import cats.data._

        def program: Unit = {
          val environment = Environment(mock[HttpClient], Config(Nil))

          type ST[T] = StateT[IO, Cache[UUID, User], T]
          type WR[T] = WriterT[ST, Log, T]
          type RT[T] = ReaderT[WR, Environment, T]

          val env = implicitly[Env[RT]]
          val service = new UserService[RT]

          val res = for {
            config <- env.reader(_.config)
            _ <- service.preCacheUsers(config.userUuids)
          } yield ()

          val (cache, (logs, _)) = res
            .run(environment)       // run Reader
            .run                    // run Writer
            .run(Cache(Map.empty))  // run State
            .unsafeRunSync()
        }
      }
      */
    }
  }
}