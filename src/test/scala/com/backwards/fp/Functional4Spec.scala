package com.backwards.fp

import scala.concurrent.{ExecutionContext, Future}
import scala.util.Try
import cats.implicits._
import monix.eval.Task
import org.scalatest.matchers.must.Matchers
import org.scalatest.wordspec.AnyWordSpec

/**
 * [[https://www.matfournier.com/2020-01-10-getting-funcey-part1-types/ Series of 5 FP articles]]
 *
 * [[https://www.matfournier.com/2020-01-28-getting-funcey-part4-traverse/ Part 4]]
 *
 * You've bought into effects and start using them. Inevitably, you will start running into some horrendous type signatures.
 * The first time it happens you will throw your hands up in despair.
 * E.g take list of urls, map some side-effecting function over it to make an HTTP request, and then maps some other function over it for parsing. You end up with something like:
 * {{{
 *  val result: List[Future[Either[ParsingError, User]]]
 * }}}
 *
 */
class Functional4Spec extends AnyWordSpec with Matchers {
  type Or[A, B] = Either[A, B]

  final case class ServiceError(message: String)

  sealed trait Request

  case class ValidRequest(value: String) extends Request

  case object BatchFail extends Request

  final case class BatchRequest(requests: List[Request])

  /* Raw, unparsed response from the client */
  final case class RawResponse(value: String)

  /* Successful parsed case */
  final case class ParsedResponse(value: Int)

  object ParsedResponse {
    val parse: String => Try[ParsedResponse] =
      s => Try(ParsedResponse(s.toInt))
  }

  final case class BatchResponse(responses: List[RawResponse])

  object Http {
    def getBatch(batch: BatchRequest)(implicit ec: ExecutionContext): Future[BatchResponse] =
      Future(BatchResponse(batch.requests.map(handleRequest)))

    private def handleRequest(request: Request): RawResponse =
      request match {
        case v: ValidRequest => RawResponse(v.value)
        case BatchFail       => throw new Exception("Http Error")
      }
  }

  "Traverse and Sequence" should {
    "be described" in {
      val x: Option[List[Int]] = List(1.some, 2.some).sequence
      x mustBe List(1, 2).some

      List(1.some, none, 2.some).sequence mustBe none

      val y: Option[List[Int]] = List(1.some, 2.some).traverse(identity)
      y mustBe List(1, 2).some

      List(1.some, 2.some).sequence mustBe List(1.some, 2.some).traverse(identity)
    }
  }

  "Monoid" should {
    "in terms of Option be described so that we can use its idea next with Traverse" in {
      List(1.some, 2.some, 3.some).combineAll mustBe 6.some

      List(1.some, 2.some, None, 3.some).combineAll mustBe 6.some
      // Some(6) - The Monoid for [Int] treats None as 0
    }

    "in terms of Either" in {
      List(1.asRight[String], 2.asRight[String]).combineAll mustBe 3.asRight // Either[String, Int]

      List(1.asRight[String], "boop".asLeft[Int]).combineAll mustBe "boop".asLeft // Either[String, Int] i.e. very different than option!
    }

    "using Monoid for Either[E, A] and Monoid for List[A]" in {
      val a = List(1).asRight[String]
      val b = List(2).asRight[String]
      val c = "bop".asLeft[List[Int]]

      val good = List(a, b).combineAll
      good mustBe List(1, 2).asRight // Either[String, List[Int]]

      val bad = List(a,b,c).combineAll
      bad mustBe "bop".asLeft // Either[String, List[Int]]
    }
  }

  "Evaluation vs Execution" should {
    "be described" in {
      def foo(i: Int): Task[Int] = {
        println("evaluation")

        Task {
          println(s"execution: $i")
          i
        }
      }

      val triggersEvaluation: Task[Int] = foo(1)

      import monix.execution.Scheduler.Implicits.global

      val triggersExecution = triggersEvaluation.runSyncUnsafe()

      /*
      In "foo" we have some side-effect being evaluated outside of our data structure.
      It gets printed when we evaluate foo. But when we evaluate foo we are really building a data structure called Task.
      If we ended our program without ever executing anything, the Task is not interpretted.
      Whatever side-effect is embedded in the Task (in our case, printing to the screen but it could be DB update or whatever) is never run.
      Only when we execute (interpret) our data structure at the end of the world via runSyncUnsafe or unsafeRunSync or runtoFuture or others, does this actually happen.
      */
    }
  }

  "Traversing/Sequencing over a batch of HTTP requests" should {
    "be demoed" in {
      import scala.concurrent.ExecutionContext.Implicits.global

      /*
      We know there are three distinct parts of this problem:
      - making the http request
      - expanding the batchResponse into List[Response]
      - parsing each response
      - doing all of the above to a list of urls
      */

      /*
      We want to factor out our effects as much as possible.
      We don't want effects on our inputs.
      We want to break it down into elements that we put together somehow (mapping, flatMapping, etc) into a bigger program.
      I don't want to take in a List[Responses], or a Future[Response] - I just want Response.

      In the following we will expand a BatchResponse into a number of parsed responses:
      */
      def parseResponse(batchResponse: BatchResponse, parse: String => Try[ParsedResponse]): ServiceError Or List[ParsedResponse] = {
        batchResponse
          .responses
          .traverse(rawResponse => parse(rawResponse.value).toEither)
          .leftMap(t => ServiceError(t.getMessage))
      }

      // Function that makes an http request and attempts to parse the response
      // def getAndParseBatchBetter(batch: BatchRequest): Future[ServiceError Or NonEmptyList[ParsedResponse]] = ???
      def getAndParseBatch(batch: BatchRequest): Future[ServiceError Or List[ParsedResponse]] =
        Http.getBatch(batch).map(batchResponse => parseResponse(batchResponse, ParsedResponse.parse))

      def get(batch: BatchRequest): Future[BatchResponse] =
        Http.getBatch(batch)

      def parse(batchResponse: BatchResponse): ServiceError Or List[ParsedResponse] =
        parseResponse(batchResponse, ParsedResponse.parse)

      def requestBatchUsingTraverseThatWontWork(requests: List[Request], batchSize: Int): Future[ServiceError Or List[ParsedResponse]] = {
        val batchedRequests: List[List[Request]] =
          requests.grouped(batchSize).toList

        val batches: List[BatchRequest] =
          batchedRequests map BatchRequest

        val resultThatWeWantButWrongReturnType: Future[List[ServiceError Or List[ParsedResponse]]] = {
          batches.traverse(batchRequest => get(batchRequest).map(parse))

          // OR
          // batches traverse getAndParseBatch // TODO: batches traverse (get andThen parse)
        }

        ???
      }

      def requestBatchUsingTraverseThatWillWork(requests: List[Request], batchSize: Int): Future[ServiceError Or List[ParsedResponse]] = {
        val batchedRequests: List[List[Request]] =
          requests.grouped(batchSize).toList

        val batches: List[BatchRequest] =
          batchedRequests map BatchRequest

        val resultThatWeWantButWrongReturnType: Future[ServiceError Or List[ParsedResponse]] =
          batches traverse getAndParseBatch map(_.combineAll) // TODO: batches traverse (get andThen parse)

        ???
      }

      def requestBatchUsingMap(requests: List[Request], batchSize: Int): Future[ServiceError Or List[ParsedResponse]] = {
        val batchedRequests: List[List[Request]] =
          requests.grouped(batchSize).toList

        val batches: List[BatchRequest] =
          batchedRequests map BatchRequest

        val result: List[Future[ServiceError Or List[ParsedResponse]]] =
          batches map getAndParseBatch // TODO: batches map get map parse

        ???
      }
    }
  }
}