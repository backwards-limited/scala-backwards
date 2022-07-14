package tech.backwards.graphql

import caliban.client.Operations.RootQuery
import caliban.client.{CalibanClientError, SelectionBuilder}
import cats.effect.IO
import sttp.client3.UriContext
import sttp.client3.asynchttpclient.cats.AsyncHttpClientCatsBackend
import sttp.client3.asynchttpclient.zio.AsyncHttpClientZioBackend
import sttp.model.Uri
import zio.{Exit, Unsafe, ZIO}
import org.scalatest.matchers.must.Matchers
import org.scalatest.wordspec.AnyWordSpec
import tech.backwards.graphql.TrainClient.{Query, Searchable, Station, Timetable, TrainInStation}

class CalibanClientSpec extends AnyWordSpec with Matchers {
  "Caliban Client to Deutsche Bahn" should {
    val uri: Uri =
      uri"https://api.deutschebahn.com/free1bahnql/v1/graphql"

    /*"query via Sttp ZIO" in {
      val query: SelectionBuilder[RootQuery, List[(String, Boolean)]] =
        Query.search(Some("Berlin Ostbahnhof")) {
          Searchable.stations {
            Station.name ~ Station.hasWiFi
          }
        }

      val z: ZIO[Any, Throwable, List[(String, Boolean)]] =
        AsyncHttpClientZioBackend().flatMap(
          _.send(query.toRequest(uri)).map(_.body).absolve
        )

      val result: List[(String, Boolean)] =
        Unsafe.unsafe { implicit u =>
          zio.Runtime.default.unsafe.run(
            z
          ).getOrThrowFiberFailure()
        }

      println(result)
    }*/

    /*"more complex query via Sttp ZIO" in {
      val trainInStation =
        TrainInStation.`type` ~
        TrainInStation.platform ~
        TrainInStation.trainNumber ~
        TrainInStation.time ~
        TrainInStation.stops

      val timeTable =
        Station.timetable {
          Timetable.nextDepatures {
            trainInStation
          } ~
          Timetable.nextArrivals {
            trainInStation
          }
        }

      val query =
        Query.search(Some("Berlin Ostbahnhof")) {
          Searchable.stations {
            Station.name ~
            Station.hasWiFi ~
            timeTable
          }
        }

      val z =
        AsyncHttpClientZioBackend().flatMap(
          _.send(query.toRequest(uri)).map(_.body).absolve
        )

      val runtime: zio.Runtime[Any] =
        zio.Runtime.default

      val result: List[(String, Boolean, (List[(String, String, String, String, List[String])], List[(String, String, String, String, List[String])]))] =
        Unsafe.unsafe(implicit u =>
          runtime.unsafe.run(z).getOrThrow()
        )

      println(result)
    }*/

    "query via Sttp IO" in {
      import cats.effect.unsafe.implicits.global

      val query: SelectionBuilder[RootQuery, List[(String, Boolean)]] =
        Query.search(Some("Berlin Ostbahnhof"))(
          Searchable.stations(
            Station.name ~ Station.hasWiFi
          )
        )

      val io: IO[Either[CalibanClientError, List[(String, Boolean)]]] =
        AsyncHttpClientCatsBackend[IO]().flatMap(
          _.send(query.toRequest(uri)).map(_.body)
        )

      val Right(result) =
        io.unsafeRunSync()

      println(result)
    }
  }
}