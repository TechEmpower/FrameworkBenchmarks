package spray.examples

import akka.actor._
import scala.concurrent.duration._
import spray.can.Http
import spray.json._
import spray.http._
import spray.util._
import MediaTypes._
import HttpMethods._
import StatusCodes._

class BenchmarkService extends Actor {
  import context.dispatcher
  import Uri._
  import Uri.Path._
  val message = "Hello, World!".getAsciiBytes
  val unknownResource = HttpResponse(NotFound, entity = "Unknown resource!")

  def fastPath: Http.FastPath = {
    case HttpRequest(GET, Uri(_, _, Slash(Segment(x, Path.Empty)), _, _), _, _, _) =>
      x match {
        case "json" =>
          val json = JsObject("message" -> JsString("Hello, World!"))
          HttpResponse(entity = HttpEntity(ContentTypes.`application/json`, json.compactPrint))
        case "plaintext" =>
          HttpResponse(entity = HttpEntity(ContentTypes.`text/plain`, message))
        case _ => unknownResource
      }
  }

  def receive = {
    // when a new connection comes in we register ourselves as the connection handler
    case _: Http.Connected => sender ! Http.Register(self, fastPath = fastPath)

    case HttpRequest(GET, Path("/"), _, _, _) => sender ! HttpResponse(
      entity = HttpEntity(MediaTypes.`text/html`,
        <html>
          <body>
            <h1>Tiny <i>spray-can</i> benchmark server</h1>
            <p>Defined resources:</p>
            <ul>
              <li><a href="/json">/json</a></li>
              <li><a href="/plaintext">/plaintext</a></li>
              <li><a href="/stop">/stop</a></li>
            </ul>
          </body>
        </html>.toString()
      )
    )

    case HttpRequest(GET, Path("/stop"), _, _, _) =>
      sender ! HttpResponse(entity = "Shutting down in 1 second ...")
      context.system.scheduler.scheduleOnce(1.second) { context.system.shutdown() }

    case _: HttpRequest => sender ! unknownResource
  }
}
