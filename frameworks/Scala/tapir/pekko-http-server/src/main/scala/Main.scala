import io.circe.generic.auto.*
import org.apache.pekko.actor.ActorSystem
import org.apache.pekko.http.scaladsl.Http
import sttp.model.HeaderNames
import sttp.tapir.*
import sttp.tapir.json.circe.*
import sttp.tapir.server.pekkohttp.PekkoHttpServerInterpreter

import scala.concurrent.Future

@main def tapirPekkoServerBenchmark(): Unit = {
  implicit val actorSystem: ActorSystem = ActorSystem()
  import actorSystem.dispatcher

  val STATIC_SERVER_NAME = "tapir-pekko-http-server"

  val plainTextMessage: String = "Hello, World!"

  given Schema[Payload] = Schema.derived

  val plaintextRoute =
      endpoint.get
        .in("plaintext")
        .out(header(HeaderNames.Server, STATIC_SERVER_NAME))
        .out(stringBody)
        .serverLogic[Future] { _ =>
          Future.successful(Right(plainTextMessage))
        }

  val jsonRoute =
      endpoint.get
        .in("json")
        .out(header(HeaderNames.Server, STATIC_SERVER_NAME))
        .out(jsonBody[Payload])
        .serverLogic[Future] { _ =>
          Future.successful(Right(Payload(plainTextMessage)))
        }

  val route = PekkoHttpServerInterpreter().toRoute(List(plaintextRoute, jsonRoute))

  Http().newServerAt("0.0.0.0", 8080).bindFlow(route)
}