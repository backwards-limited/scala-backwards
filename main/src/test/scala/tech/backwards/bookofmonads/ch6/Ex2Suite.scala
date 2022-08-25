package tech.backwards.bookofmonads.ch6

import cats.implicits.{catsSyntaxFlatMapOps, catsSyntaxOptionId, none}
import cats.{Functor, Monad}
import munit.FunSuite

/**
 * Reader Monad
 *
 * Consider the following piece of code, in which cfg represents a configuration value.
 * Parts of this configuration are threaded to all three calls in its body:
 * {{{
 *    handle :: Config -> Request -> Response
 *    handle cfg req =
 *      produceResponse cfg (initializeHeader cfg) (getArguments cfg req)
 * }}}
 *
 * Clearly, there is a lot of boilerplate here. With the Reader monad, we can remove most of it:
 * {{{
 *    handle :: Request -> Reader Config Response
 *    handle req = do header <- initializeHeader
 *                    args   <- getArguments req
 *                    produceResponse header args
 * }}}
 *
 * Another way to look at Reader is to think of it as introducing an implicit parameter, which is threaded under the covers by the compiler.
 * But in contrast to real implicit parameters, such as the ones in Scala, you can only have one per computation.
 *
 * The main, primitive operation for Reader is, in fact, the one that obtains — or "reads" — the environment:
 * {{{
 *    ask :: Reader r r
 * }}}
 *
 * From this operation, we derive the subtly different asks, which applies a function to the environment before returning it:
 * {{{
 *    asks :: (r -> a) -> Reader r a
 *    asks f = f <$> ask
 * }}}
 */
class Ex2Suite extends FunSuite {
  test("Reader Monad - equivalent to Kleisli[Id, A, B]") {
    import cats.data.Reader

    // First config - To configure database access
    final case class DbConfig(url: String, user: String, pass: String)

    trait Db

    object Db {
      val fromDbConfig: Reader[DbConfig, Db] = // Equivalent to: Kleisli[Id, DbConfig, Db]
        Reader(dbConfig => new Db {}) // Dummy implementation
    }

    // Second config - To configure an API for accessing data over the web
    final case class ServiceConfig(addr: String, port: Int)

    trait Service

    object Service {
      val fromServiceConfig: Reader[ServiceConfig, Service] = // Equivalent to: Kleisli[Id, ServiceConfig, Service]
        Reader(serviceConfig => new Service {}) // Dummy implementation
    }

    // Wrapping (global) application configuration
    final case class AppConfig(dbConfig: DbConfig, serviceConfig: ServiceConfig)

    // App with dependency injection
    final class App(db: Db, service: Service)

    def appFromAppConfig: Reader[AppConfig, App] = // Equivalent to: Kleisli[Id, AppConfig, App]
      for {
        db <- Db.fromDbConfig.local[AppConfig](_.dbConfig)
        sv <- Service.fromServiceConfig.local[AppConfig](_.serviceConfig)
      } yield new App(db, sv)
  }

  test("ReaderT Monad - equivalent to Kleisli[Option, A, B]") {
    import cats.data.ReaderT

    // First config - To configure database access
    final case class DbConfig(url: String, user: String, pass: String)

    trait Db

    object Db {
      val fromDbConfig: ReaderT[Option, DbConfig, Db] = // Equivalent to: Kleisli[Option, DbConfig, Db]
        ReaderT(dbConfig => new Db {} some) // Dummy implementation - We would return None if configuration fails
    }

    // Second config - To configure an API for accessing data over the web
    final case class ServiceConfig(addr: String, port: Int)

    trait Service

    object Service {
      val fromServiceConfig: ReaderT[Option, ServiceConfig, Service] = // Equivalent to: Kleisli[Option, ServiceConfig, Service]
        ReaderT(serviceConfig => new Service {} some) // Dummy implementation - We would return None if configuration fails
    }

    // Wrapping (global) application configuration
    final case class AppConfig(dbConfig: DbConfig, serviceConfig: ServiceConfig)

    // App with dependency injection
    final class App(db: Db, service: Service)

    def appFromAppConfig: ReaderT[Option, AppConfig, App] = // Equivalent to: Kleisli[Option, AppConfig, App]
      for {
        db <- Db.fromDbConfig.local[AppConfig](_.dbConfig)
        sv <- Service.fromServiceConfig.local[AppConfig](_.serviceConfig)
      } yield new App(db, sv)
  }

  test("Simple Reader implementation") {
    type Reader[R, A] = R => A

  }
}