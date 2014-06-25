package controllers

import play.api.mvc._
import play.api.libs.json.Json

object Application extends Controller {

  def json() = Action {
    Ok(Json.obj("message" -> "Hello, World!"))
  }

}