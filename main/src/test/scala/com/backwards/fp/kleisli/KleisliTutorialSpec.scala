package com.backwards.fp.kleisli

import scala.util.Success
import cats.effect.unsafe.implicits.global
import org.scalatest.matchers.must.Matchers
import org.scalatest.wordspec.AnyWordSpec

class KleisliTutorialSpec extends AnyWordSpec with Matchers {
  "Tutorial" should {
    /** [[https://medium.com/@supermanue/understanding-kleisli-in-scala-9c42ec1a5977]] */
    "first" in {
      import cats.data.Kleisli
      import cats.implicits._

      val parse: String => Option[Int] =
        s => if (s.matches("-?[0-9]+")) Option(s.toInt) else None

      val reciprocal: Int => Option[Double] =
        i => if (i != 0) Option(1.0 / i) else None

      parse("7") mustBe Option(7)
      parse("casa") mustBe None
      reciprocal(7) mustBe Option(0.14285714285714285)
      reciprocal(0) mustBe None

      // FUNCTION COMPOSITION
      def parseAndReciprocal(s: String): Option[Double] =
        parse(s).flatMap(reciprocal)

      parseAndReciprocal("7") mustBe Option(0.14285714285714285)

      // USING KLEISLI
      // final case class Kleisli[F[_], A, B](val run: scala.Function1[A, F[B]])
      // final case class Kleisli[F[_], A, B](val run: A => F[B])
      val parseKleisli: Kleisli[Option, String, Int] =
        Kleisli((s: String) => if (s.matches("-?[0-9]+")) Option(s.toInt) else None)

      val reciprocalKleisli: Kleisli[Option, Int, Double] =
        Kleisli(reciprocal)

      parseKleisli("7") mustBe Option(7)
      parseKleisli.run("7") mustBe Option(7)
      parseKleisli("casa") mustBe None
      reciprocalKleisli(7) mustBe Option(0.14285714285714285)
      reciprocalKleisli(0) mustBe None

      val parseAndReciprocalKleisli = parseKleisli andThen reciprocalKleisli
      parseAndReciprocalKleisli("7") mustBe Option(0.14285714285714285)
    }

    /** [[https://medium.com/@supermanue/real-world-applications-of-scala-kleisli-dependency-injection-36ef589ee77b]] */
    "second" in {
      import scala.util.{Success, Try}
      import cats.data.Kleisli
      import cats.effect.{IO, Sync}

      final case class Shipment(ref: String, status: String)

      sealed trait ShipmentStorage[F[_]] {
        def retrieveShipment(shipmentReference: String): F[Shipment]
      }

      class ShipmentStorageImpl1 extends ShipmentStorage[IO] {
        override def retrieveShipment(shipmentReference: String): IO[Shipment] = IO.pure(Shipment(shipmentReference, "OK"))
      }

      class ShipmentStorageImpl2[F[_]: Sync] extends ShipmentStorage[F] {
        override def retrieveShipment(shipmentReference: String): F[Shipment] = Sync[F].delay(Shipment(shipmentReference, "OK"))
      }

      class ShipmentStorageImpl3 extends ShipmentStorage[Try] {
        override def retrieveShipment(shipmentReference: String): Try[Shipment] = Success(Shipment(shipmentReference, "OK"))
      }

      type Operation[F[_], R] = Kleisli[F, ShipmentStorage[F], R]

      object OperationService {
        def getShipment[F[_]](shipmentReference: String): Operation[F, Shipment] =
          Kleisli { shipmentStorage: ShipmentStorage[F] =>
            // log stuff before accessing
            shipmentStorage.retrieveShipment(shipmentReference)
            // process result after accessing
          }

        def storeShipment[F[_]](shipmentReference: String): Operation[F, Shipment] = ???

        def deleteShipment[F[_]](shipmentReference: String): Operation[F, Shipment] = ???
      }

      val shipmentReference = "1234"
      val storage1 = new ShipmentStorageImpl1
      val res: IO[Shipment] = OperationService.getShipment(shipmentReference)(storage1)
      res.unsafeRunSync() mustBe Shipment(shipmentReference, "OK")

      val storage2 = new ShipmentStorageImpl2[IO]
      val res2: IO[Shipment] = OperationService.getShipment(shipmentReference).run(storage2)
      res2.unsafeRunSync() mustBe Shipment(shipmentReference, "OK")

      val storage3 = new ShipmentStorageImpl3
      val res3: Try[Shipment] = OperationService.getShipment(shipmentReference)(storage3)
      res3 mustBe Success(Shipment(shipmentReference, "OK"))
    }

    /** [[https://medium.com/@supermanue/real-world-applications-of-scala-kleisli-configuration-ba39e97befec]] */
    "three" in {
      import scala.util.Try
      import cats.data.Kleisli
      import cats.implicits._

      final case class DbConfig(url: String, user: String, pass: String)

      sealed trait Db

      object DbImpl {
        val fromDbConfig: Kleisli[Try, DbConfig, Db] =
          Kleisli(dbConfig => Try(new Db {}))
      }

      final case class ServiceConfig(addr: String, port: Int)

      sealed trait Service

      object ServiceImpl {
        val fromServiceConfig: Kleisli[Try, ServiceConfig, Service] =
          Kleisli(serviceConfig => Try(new Service {}))
      }

      final case class AppConfig(url: String, user: String, pass: String, addr: String, port: Int)

      class App(db: Db, service: Service)

      def appFromAppConfig: Kleisli[Try, AppConfig, App] =
        for {
          db <- DbImpl.fromDbConfig.local[AppConfig](appConfig => DbConfig(appConfig.url, appConfig.user, appConfig.pass))
          sv <- ServiceImpl.fromServiceConfig.local[AppConfig](appConfig => ServiceConfig(appConfig.addr, appConfig.port))
        } yield new App(db, sv)

      val appConfig: AppConfig = AppConfig("url", "user", "pass", "addr", 80)

      val Success(app) = appFromAppConfig(appConfig)

      app mustBe an [App]
    }
  }
}