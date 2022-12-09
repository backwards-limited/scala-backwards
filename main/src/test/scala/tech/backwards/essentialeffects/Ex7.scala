package tech.backwards.essentialeffects

import cats.effect._
import scala.io.Source
import tech.backwards.essentialeffects.debug._

object EarlyRelease extends IOApp {
  def run(args: List[String]): IO[ExitCode] =
    dbConnectionResource
      .use(conn =>
        conn.query("SELECT * FROM users WHERE id = 12").log
      )
      .as(ExitCode.Success)

  val dbConnectionResource: Resource[IO, DbConnection] =
    for {
      config <- configResource
      conn <- DbConnection.make(config.connectURL)
    } yield conn

  /* The following does not perform early release
  lazy val configResource: Resource[IO, Config] =
    for {
      source <- sourceResource
      config <- Resource.eval(Config.fromSource(source))
    } yield config*/

  lazy val configResource: Resource[IO, Config] =
    Resource.eval(sourceResource.use(Config.fromSource))

  lazy val sourceResource: Resource[IO, Source] =
    Resource.make(
      IO(s"> opening Source to config").log *> IO(Source.fromString(config))
    )(source =>
      IO(s"< closing Source to config").log *> IO(source.close)
    )

  val config = "exampleConnectURL"
}

case class Config(connectURL: String)

object Config {
  def fromSource(source: Source): IO[Config] =
    for {
      config <- IO(Config(source.getLines().next()))
      _ <- IO(s"read $config").log
    } yield config
}

trait DbConnection {
  def query(sql: String): IO[String] // Why not!?
}

object DbConnection {
  def make(connectURL: String): Resource[IO, DbConnection] =
    Resource.make(
      IO(s"> opening Connection to $connectURL").log *> IO(
        new DbConnection {
          def query(sql: String): IO[String] =
            IO(s"""(results for SQL "$sql")""")
        }
      )
    )(_ =>
      IO(s"< closing Connection to $connectURL").log.void
    )
}