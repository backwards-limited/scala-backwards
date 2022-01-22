package tech.backwards.fp.kleisli

import scala.util.Random
import cats.arrow.Arrow.ops.toAllArrowOps
import cats.data.Kleisli
import cats.effect.IO
import cats.effect.unsafe.implicits.global
import org.scalatest.matchers.must.Matchers
import org.scalatest.wordspec.AnyWordSpec

/**
 * [[https://medium.com/iadvize-engineering/request-tracing-in-microservices-architecture-a-kleisli-tale-aae8daaa61f]]
 */
class ContextualKleisliSpec extends AnyWordSpec with Matchers {
  "Kleisli" should {
    "provide a context for tracing" in {
      final case class CorrelationId(value: String)

      object CorrelationId {
        def apply: CorrelationId = CorrelationId("blah")
      }

      type RIO[A] = Kleisli[IO, CorrelationId, A]

      object RIO {
        def unit: Kleisli[IO, CorrelationId, Unit]                              = Kleisli.pure(())
        def liftF[B](x: IO[B]): Kleisli[IO, CorrelationId, B]                   = Kleisli.liftF[IO, CorrelationId, B](x)
        def pure[B](x:  B): Kleisli[IO, CorrelationId, B]                       = Kleisli.pure(x)
        def apply[B](f: CorrelationId => IO[B]): Kleisli[IO, CorrelationId, B]  = Kleisli(f)
        def ask: Kleisli[IO, CorrelationId, CorrelationId]                      = Kleisli.ask[IO, CorrelationId]
      }

      final case class EntityA(id: String, idOfB: EntityBID)

      trait ServiceA {
        def createA(a: EntityA): RIO[String]
      }

      final case class EntityBID()

      final case class EntityB()

      trait ClientB {
        def getB(bId: EntityBID): RIO[EntityB]
      }

      trait Logger {
        def info(msg: String): RIO[Unit] = RIO { cid =>
          IO(println(s"[$cid] $msg"))
        }
      }

      case class ServiceAImpl(clientB: ClientB, logger: Logger) extends ServiceA {
        override def createA(a: EntityA): RIO[String] = {
          for {
            entityB    <- clientB.getB(a.idOfB)
            processed  <- somePrivateBusinessLogic(entityB)
            result     <- someMoreBusinessLogic(processed)
            correlationId           <- RIO.ask
            _          <- logger.info(s"[$correlationId] Successfully processed entity ${a.id}")
          } yield result
        }

        def somePrivateBusinessLogic(entityB: EntityB): RIO[String] = ???

        def someMoreBusinessLogic(p: String): RIO[String] = ???
      }

      final case class Body() {
        def decodeJson[A]: A = ???
      }

      final case class HttpRequest(headers: Map[String, String], body: Body)

      final case class Route(run: HttpRequest => IO[String])

      case class ApiA(serviceA: ServiceA) {
        def post = Route { request =>
          val correlationId = request.headers.get("X-Correlation-Id")
            .map(CorrelationId.apply)
            .getOrElse(CorrelationId.apply)

          serviceA.createA(request.body.decodeJson[EntityA])
            .run(correlationId)
        }

        val result: String = post.run(HttpRequest(Map(), Body())).unsafeRunSync()
      }
    }
  }
}