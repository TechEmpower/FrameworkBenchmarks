package controllers

import play.api.mvc._
import play.api.libs.json.Json

object Application extends Controller {

  def json() = Action {
    Ok(Json.obj("message" -> "Hello, World!"))
  }

  def plaintext() = Action {
    import java.util.Date
    import java.text.SimpleDateFormat

    val sdf = new SimpleDateFormat("EEE, MMM d yyyy HH:MM:ss z")
    Ok("Hello, World!")
      .withHeaders(
        DATE -> sdf.format(new Date()),
        SERVER -> "Play Framework 2.3.3")
  }
}