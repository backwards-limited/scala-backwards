package com.backwards.fp.pancakes

import fs2.Pipe
import monix.eval.Task
import sttp.capabilities.WebSockets
import sttp.capabilities.fs2.Fs2Streams
import sttp.tapir._
import sttp.tapir.server.ServerEndpoint
//import com.backwards.fp.pancakes.Json._
//import sttp.tapir.CodecFormat.Json
//import sttp.tapir.json.circe.TapirJsonCirce
//import sttp.tapir.Codec.JsonCodec
//import sttp.tapir.json.circe._
//import io.circe.generic.auto._

object Server {
  // A value which describes in detail a single query input
  val pansQueryInput: EndpointInput.Query[Int] =
    query[Int]("pans") // i.e. number of frying pans
      .description("The number of frying pans to use in parallel")
      .example(2)
      .validate(Validator min 1)

  val pancakesEndpoint: Endpoint[Int, String, Pipe[Task, PancakeIngredient, PancakeStatus], WebSockets with Fs2Streams[Task]] = ??? // TODO - Compilation error
    /*endpoint
      .in("pancakes")
      .in(pansQueryInput)
      .errorOut(stringBody)
      .out(webSocketBody[PancakeIngredient, CodecFormat.Json, PancakeStatus, CodecFormat.Json](Fs2Streams[Task]))*/

  // Combining the endpoint description with server logic
  val pancakesServerEndpoint: ServerEndpoint[Int, String, Pipe[Task, PancakeIngredient, PancakeStatus], WebSockets with Fs2Streams[Task], Task] =
    pancakesEndpoint.serverLogic(ServerLogic.bakePancakes)
}