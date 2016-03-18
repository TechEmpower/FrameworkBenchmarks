package controllers

import javax.inject.Singleton
import akka.util.CompactByteString
import play.api.http.HttpEntity.Strict
import play.api.mvc._
import play.api.libs.json.Json
import play.mvc.Http

@Singleton
class Application extends Controller {
  val header = ResponseHeader(OK, Map(Http.HeaderNames.SERVER -> "EXAMPLE"))
  implicit val helloWorldWrites = Json.writes[HelloWorld]

  def getJsonMessage = Action {
    val helloWorld = HelloWorld(message = "Hello, World!")
    Result(header, Strict(CompactByteString(Json.toJson(helloWorld).toString()), Some("application/json")))
  }

  val plaintext = Action {
    Result(header, Strict(CompactByteString("Hello, World!"), Some("text/plain")))
  }
}

case class HelloWorld(message: String)