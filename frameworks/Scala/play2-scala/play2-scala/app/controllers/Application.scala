package controllers

import javax.inject.Singleton

import play.api.mvc._
import play.api.libs.json.Json

@Singleton
class Application extends Controller {

  // Test seems picky about headers.  Doesn't like character set being there for JSON.  Always wants Server header set.
  // There is a Filter which adds the Server header for all types.  Below I set Content-Type as needed to get rid of
  // warnings.

  // Easy ones
  case class HelloWorld(message: String)
  implicit val helloWorldWrites = Json.writes[HelloWorld]

  def getJsonMessage = Action {
    val helloWorld = HelloWorld(message = "Hello, World!")
    Ok(Json.toJson(helloWorld)).withHeaders(CONTENT_TYPE -> "application/json")
  }

  val plaintext = Action {
    // default headers are correct according to docs: charset included.
    // BUT the test harness has a WARN state and says we don't need it.
    Ok("Hello, World!").withHeaders(CONTENT_TYPE -> "text/plain")
  }
}
