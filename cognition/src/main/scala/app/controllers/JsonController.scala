package controllers

import org.cognition._

class JsonController extends Controller {

  get("json")(_ => render.json(Map("message" -> "Hello, World!")))

}
