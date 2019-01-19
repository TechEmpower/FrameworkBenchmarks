package controllers

import javax.inject.{ Inject, Singleton }

import play.api.mvc._
import play.api.libs.json.Json
import play.mvc.Http

@Singleton
class Application @Inject() (cc: ControllerComponents)
extends AbstractController(cc) {

  implicit final val helloWorldWrites = Json.writes[HelloWorld]

  def getJsonMessage = Action {
    Ok( Json.toJson(HelloWorld()) )
  }

  val plaintext = Action {
    Ok("Hello, World!").as(play.api.http.MimeTypes.TEXT)
  }
}

case class HelloWorld(message: String = "Hello, World!")
