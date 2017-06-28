package controllers

import javax.inject.{ Inject, Singleton }

import play.api.mvc._
import play.api.libs.json.Json
import play.mvc.Http

@Singleton
class Application @Inject() (cc: ControllerComponents)
extends AbstractController(cc) {

  implicit val helloWorldWrites = Json.writes[HelloWorld]

  def getJsonMessage = Action {
    val helloWorld = HelloWorld(message = "Hello, World!")
    Ok(Json.toJson(helloWorld)).withHeaders(Http.HeaderNames.SERVER -> "Play Framework")
  }

  val plaintext = Action {
    Ok("Hello, World!").withHeaders(Http.HeaderNames.SERVER -> "Play Framework")
  }
}

case class HelloWorld(message: String)