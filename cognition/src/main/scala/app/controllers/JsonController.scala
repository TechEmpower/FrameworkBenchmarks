package controllers

import org.cognition._

class JsonController extends Controller {

  get("json")(_ => render.json(message))

  private[this] final val message = Map("message" -> "Hello, World!")

}
