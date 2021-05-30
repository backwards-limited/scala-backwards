package com.backwards.fp

import scala.concurrent.duration._
import scala.concurrent.{ExecutionContext, Future}
import cats.{Monad, MonadError}
import cats.effect.{Concurrent, Deferred, ExitCode, IO, IOApp, Sync}
import cats.implicits._
import io.circe.generic.auto._
import io.circe.syntax.EncoderOps
import monix.eval.Task
import retry.RetryPolicies.{exponentialBackoff, limitRetries}
import retry.{RetryPolicy, Sleep, retryingOnAllErrors}
import sttp.client3.asynchttpclient.cats.AsyncHttpClientCatsBackend
import sttp.client3.circe.asJson
import sttp.client3.{ResponseException, SttpBackend, UriContext, basicRequest}
import org.http4s.circe.jsonEncoder
import org.http4s.dsl.Http4sDsl
import org.http4s.{HttpRoutes, Response}
import org.scalatest.matchers.must.Matchers
import org.scalatest.wordspec.AnyWordSpec
import org.typelevel.log4cats.Logger
import com.backwards.fp.{Nothing => _}

/**
 * [[https://www.matfournier.com/2020-01-10-getting-funcey-part1-types/ Series of 5 FP articles]]
 *
 * [[https://www.matfournier.com/2020-02-23-getting-funcey-part5-io/ Part 5]]
 *
 * What is an "effect?
 * Whatever differentiates "A" from "F[A]" e.g. Option, IO, EitherT.
 *
 * Note, we can keep stacking monads e.g.
 * {{{
 *  ReaderT[IO, Config, EitherT[List, Throwable, Result]]
 * }}}
 *
 *  - Reader is a function from A => B
 *  - ReaderT is a function from A => F[B], a monad transformer for functions,
 *    which is an alias for "Kleisli".
 *
 * You can keep stacking - the entire stack is still just some type F[A].
 *
 * As an example
 *
 *  - we'll look at a webservice with "Akka HTTP" and "Future"
 *  - compare to a FP approach with "Http4s" using "Cats Effect" in final tagless style
 *  - and a brief look at ZIO
 */
class Functional5Spec extends AnyWordSpec with Matchers {
  "Tagless final" should {
    "write Algebras (think Services) parameterized on some abstract F[_]" in {
      trait Console[F[_]] {
        def put(v: String): F[Unit]

        def get: F[String]
      }
      /*
      This shares the same encoding as a typeclass but is not a typeclass since we may define more than one interpretation for a given type.
      A downside of the Tagless Final approach is that we have to differentiate between algebras and typeclasses ourselves in type signatures.

      If the above was a typeclass we would take some parameterized type, and use that in the functions e.g.
      trait ConsoleTypeClass[F[A]] {
        def put(a: A): F[Unit]

        def get(a: A): F[A] // Though this function now really doesn't make sense
      }
      */

      // We can now create an interpreter on some concrete type, in this case Task but it could have been IO or ZIO:
      implicit object TaskInterpretter extends Console[Task] {
        def put(v: String): Task[Unit] = Task delay println(v)

        def get: Task[String] = Task eval scala.io.StdIn.readLine()
      }
    }
  }

  "Akka HTTP example" should {
    final case class ValidAuthorizeRequest()
    final case class AuthorizeResponse(version: String, authAccessToken: AuthAccessToken)

    final case class ValidAuthenticateRequest()
    final case class AuthenticateResponse()

    final case class AuthAccessToken()
    final case class SocialProfilesResponse()

    object ApiError {
      def fromResponse(code: Int, e: ResponseException[String, Exception]): Throwable = ???
    }

    "define 3 endpoints around interfacing with a Social Profile API with the following interface as a dependency of akka-http routes" in {
      trait AuthService {
        def authorize(req: ValidAuthorizeRequest): Future[AuthorizeResponse]
        def authenticate(req: ValidAuthenticateRequest): Future[AuthenticateResponse]
        def getManageableSocialProfiles(accessToken: AuthAccessToken): Future[SocialProfilesResponse]
      }

      /*
      Let's say we are using Sttp to make remote calls to the third party API.
      How would the constructors look different between the two approaches for an implementation? In the OOP case, we would have some concrete class (implementing only "authorize" as an example):
      */
      class LiveAuthService(implicit backend: SttpBackend[Future, Any], ec: ExecutionContext) extends AuthService {
        def authenticate(req: ValidAuthenticateRequest): Future[AuthenticateResponse] = ???
        def getManageableSocialProfiles(accessToken: AuthAccessToken): Future[SocialProfilesResponse] = ???

        /*
        The Future may timeout or have some other error.
        There may be some parsing error on the response.body when we try to parse the json to an AuthAccessToken, if there is, we turn that into some domain specific ApiError.
        If everything works out correctly, we construct an AuthorizeResponse.
        */
        def authorize(req: ValidAuthorizeRequest): Future[AuthorizeResponse] = {
          val authorizeBody: Map[String, String] = Map(
            // ... post body here ....
          )

          basicRequest
            .body(authorizeBody)
            .post(uri"https://authservice.com")
            .response(asJson[AuthAccessToken])
            .send(backend)
            .flatMap { response =>
              response.body.fold(
                error => Future.failed(ApiError.fromResponse(response.code.code, error)),
                token => Future.successful(AuthorizeResponse("oauthVersion", token))
              )
            }
        }
      }
    }

    "and the tagless version would be" in {
      trait AuthService[F[_]] {
        def authorize(req: ValidAuthorizeRequest): F[AuthorizeResponse]
        def authenticate(req: ValidAuthenticateRequest): F[AuthenticateResponse]
        def getManageableSocialProfiles(accessToken: AuthAccessToken): F[SocialProfilesResponse]
      }

      class LiveAuthService[F[_]: MonadError[*[_], Throwable]](implicit backend: SttpBackend[F, Any]) extends AuthService[F] {
        def authenticate(req: ValidAuthenticateRequest): F[AuthenticateResponse] = ???
        def getManageableSocialProfiles(accessToken: AuthAccessToken): F[SocialProfilesResponse] = ???

        /*
        The main difference (with the above Futures) is how errors are raised, which we do through the MonadError typeclass.
        To raise an error we use raiseError. To return a value, we use pure.
        */
        def authorize(req: ValidAuthorizeRequest): F[AuthorizeResponse] = {
          val authorizeBody: Map[String, String] = Map(
            // ... post body ...
          )

          basicRequest
            .body(authorizeBody)
            .post(uri"https://authservice.com")
            .response(asJson[AuthAccessToken])
            .send(backend)
            .flatMap { response =>
              response.body.fold(
                error => ApiError.fromResponse(response.code.code, error).raiseError[F, AuthorizeResponse],
                token => AuthorizeResponse("oauthVersion", token).pure[F]
              )
            }
        }
      }
      /*
      The final-tagless version has the benefit that we don't have to pass along the execution context everywhere.
      We know that remote calls can fail, which is encoded in the Failure channel of Future. But what about our F?

      We need to use a typeclass to give our F more capabilities.
      That is, we need whatever F to be supplied to have the ability to handle errors. Hence, MonadError.

      The interesting thing here is that all we know about F is that we can throw and recover from errors,
      and that F is a monad, so we will have access to flatMap,
      and since every Monad is also a Functor, we have access to map.

      We know much less than we do than the version using a concrete Future.
      Our code can only use what we have provided to F, that is, it's much more constrained than Future.
      Someone isn't going to be able to embed some arbitrary effect because the type system won't allow it.
      There is nothing stopping someone with the Future from being able to do Future(sneakyMineBitCoin(panamaAccountId)) for example.
      */

      // If we were to hook up the Algebra (service) with http4s route:
      final class AuthRoutes[F[_]: MonadError[*[_], Throwable]: Concurrent](authService: AuthService[F]) extends Http4sDsl[F] {
        import io.circe.generic.auto._
        import org.http4s.circe.CirceEntityDecoder._
        import org.http4s.circe._

        val errorHandler: PartialFunction[Throwable, F[Response[F]]] = {
          case t: Throwable => BadRequest()
        }

        val routes: HttpRoutes[F] = HttpRoutes.of[F] {
          case req @ POST -> Root / "authorize" =>
            req.decode[ValidAuthorizeRequest] { authReq =>
              authService
                .authorize(authReq)
                .flatMap(r => Ok(r.asJson))
                .recoverWith(errorHandler)
            }
        }
      }

      // We can get an instance of our Algebra as:
      object LiveAuthService {
        def make[F[_]: Sync](backend: SttpBackend[F, Any]): F[AuthService[F]] = {
          implicit val sttp: SttpBackend[F, Any] = backend

          Sync[F].delay(new LiveAuthService())
        }
      }

      // and likewise a companion object for AuthRoutes
      object AuthRoutes {
        def make[F[_]: MonadError[*[_], Throwable]: Concurrent](authService: AuthService[F]): F[AuthRoutes[F]] = {
          Concurrent[F].pure(new AuthRoutes[F](authService))
          // Sync[F].delay(new AuthRoutes[F](authService))
        }
      }

      // Then an IOApp
      object App extends IOApp {
        def run(args: List[String]): IO[ExitCode] =
          AsyncHttpClientCatsBackend.resource[IO]().use { backend =>
            for {
              authService <- LiveAuthService.make[IO](backend)
              // programs = AuthProgram.make[IO](services)
              // api <- RoutingModule.make[IO](programs)
              authRoutes <- AuthRoutes.make[IO](authService)
              /*_ <- BlazeServerBuilder[IO](scala.concurrent.ExecutionContext.Implicits.global)
                .bindHttp()
                .withHttpApp(authRoutes.routes.orNotFound)
                .serve
                .compile
                .drain*/
            } yield ExitCode.Success
          }
      }

      /*
      def newEmber(cfg: HttpServerConfig, httpApp: HttpApp[F]): Resource[F, Server] =
        EmberServerBuilder
          .default[F]
          .withHost(cfg.host)
          .withPort(cfg.port)
          .withHttpApp(httpApp)
          .build
          .evalTap(showEmberBanner[F])
       */

      /*
      Note "programs" - We could "wrap" (think of aspects) to enhance e.g.
      */
      class AuthProgram[F[_]: Logger: MonadError[*[_], Throwable]: Sleep/*: Statsd*/](authService: AuthService[F]/*, eventService: EventService[F]*/) {
        val retryPolicy: RetryPolicy[F] =
          limitRetries[F](3) |+| exponentialBackoff[F](10 milliseconds)

        def authorize(req: ValidAuthorizeRequest): F[AuthorizeResponse] =
          for {
            logger    <- Logger[F].info("started authorize request")
            response  <- retryingOnAllErrors(retryPolicy, (t: Throwable, d) => Logger[F].error(t)(d.toString))(authService.authorize(req))
            // _      <- eventService.emit(Event("SomeAuditEvent"))
            // _      <- Statsd[F].emit("SomeKey", 10)
            _         <- Logger[F].info("finished authorize request")
          } yield response
      }
      /*
      What has changed?
      Our F has more capabilities:
      - it knows about Time (required for retrying to work)
      - it knows about Logging
      - it knows about Statsd events
      - our F knows about Monad for flatMap
      - our F knows about MonadError so that retrying knows about failures
      */
    }
  }
}